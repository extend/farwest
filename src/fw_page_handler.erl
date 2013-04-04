%% [{template, Template}]
-module(fw_page_handler).

-export([init/3]).
-export([rest_init/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([resource_exists/2]).
-export([to_html/2]).
-export([to_json/2]).

-record(state, {
	auth = undefined,
	template :: module(),
	rules :: list(),
	data :: list(),
	userid = undefined
}).

init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
	State = case lists:keyfind(auth, 1, Opts) of
		{auth, AuthOpts} -> #state{auth=AuthOpts};
		false -> #state{}
	end,
	{template, Template} = lists:keyfind(template, 1, Opts),
	Rules = proplists:get_value(rules, Opts, []),
	Module = list_to_existing_atom(atom_to_list(Template) ++ "_dtl"),
	{ok, Req, State#state{template=Module, rules=Rules}}.

is_authorized(Req, State=#state{auth=undefined}) ->
	{true, Req, State};
is_authorized(Req, State=#state{auth=AuthOpts}) ->
	case fw_auth:authenticate(Req, AuthOpts) of
		{false, Req2} ->
			{{false, fw_auth:methods(AuthOpts)}, Req2, State};
		{UserID, Req2} ->
			{true, Req2, State#state{userid=UserID}}
	end.

content_types_provided(Req, State) ->
	{[
		{<<"text/html">>, to_html},
		{<<"application/json">>, to_json}
	], Req, State}.

%% Only allow UTF-8.
charsets_provided(Req, State) ->
	{[{<<"utf-8">>, 1000}, {<<"*">>, 0}], Req, State}.

resource_exists(Req, State=#state{rules=Rules}) ->
	case query_data(Rules, Req) of
		{ok, Data, Req2} ->
			{true, Req2, State#state{data=Data}};
		{error, notfound} ->
			{false, Req, State}
	end.

to_html(Req, State=#state{template=Module, data=Data}) ->
	{ok, Body} = Module:render(Data),
	{Body, Req, State}.

to_json(Req, State=#state{data=Data}) ->
	{jsx:encode(Data), Req, State}.

%% Internal.

query_data(Rules, Req) ->
	query_data(Rules, Req, []).
query_data([], Req, Acc) ->
	case get_links() of
		[] ->
			{ok, Acc, Req};
		Links ->
			[Data] = walk_list([Acc], Links, []),
			{ok, Data, Req}
	end;
query_data([Rule|Tail], Req, Acc) ->
	case query_field(Rule, Req) of
		E = {error, _} ->
			E;
		{Field, Req2} ->
			query_data(Tail, Req2, [Field|Acc])
	end.

query_field({Name, {get, Bucket, {binding, Binding}}}, Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	{Key, Req2} = cowboy_req:binding(Binding, Req),
	case fw_userdata_server:get_value(Bucket2, Key) of
		{ok, JSON} ->
			{{Name, jsx:decode(JSON)}, Req2};
		E = {error, _} ->
			E
	end;
query_field({Name, {get, Bucket, path_info}}, Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	{Key, Req2} = cowboy_req:path_info(Req),
	case fw_userdata_server:get_value(Bucket2, Key) of
		{ok, JSON} ->
			{{Name, jsx:decode(JSON)}, Req2};
		E = {error, _} ->
			E
	end;
query_field({Name, {get_all, Bucket}}, Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	{ok, ValuesList} = fw_userdata_server:get_all_values(Bucket2),
	{{Name, lists:sort(ValuesList)}, Req};
query_field({Name, {mfa, {M, F, A}}}, Req) ->
	{ok, ValuesList} = apply(M, F, A),
	{{Name, ValuesList}, Req}.

get_links() ->
	case application:get_env(farwest, links_file) of
		{ok, {priv_dir, App, Path}} ->
			{ok, Links} = code:priv_dir(App) ++ "/" ++ Path,
			Links;
		{ok, Filename} ->
			{ok, Links} = file:consult(Filename),
			Links;
		_ ->
			[]
	end.

walk_list([], _, Acc) ->
	lists:reverse(Acc);
walk_list([Value|Tail], Links, Acc) when is_list(Value) ->
	case Value of
		[Hd|_] when is_tuple(Hd) ->
			Value2 = walk_array(Value, Links, []),
			walk_list(Tail, Links, [Value2|Acc]);
		_ ->
			Value2 = walk_list(Value, Links, []),
			walk_list(Tail, Links, [Value2|Acc])
	end;
walk_list([Value|Tail], Links, Acc) ->
	walk_list(Tail, Links, [Value|Acc]).

walk_array([], _, Acc) ->
	lists:reverse(Acc);
walk_array([{Key, []}|Tail], Links, Acc) ->
	walk_array(Tail, Links, [{Key, []}|Acc]);
walk_array([{Key, Value}|Tail], Links, Acc) when is_list(Value) ->
	case Value of
		[Hd|_] when is_tuple(Hd) ->
			Value2 = walk_array(Value, Links, []),
			walk_array(Tail, Links, [{Key, Value2}|Acc]);
		_ ->
			Value2 = walk_list(Value, Links, []),
			walk_array(Tail, Links, [{Key, Value2}|Acc])
	end;
walk_array([{Key, Value}|Tail], Links, Acc) ->
	Ignore = byte_size(Key) - 5,
	case Key of
		<< KeyPrefix:Ignore/binary, "_link" >> ->
			case lists:keyfind(Key, 1, Links) of
				false ->
					walk_array(Tail, Links, [{Key, Value}|Acc]);
				{_, Bucket, ValuePrefix} ->
					PrefixSize = byte_size(ValuePrefix),
					<< ValuePrefix:PrefixSize/binary, Suffix/bits >> = Value,
					case fw_userdata_server:get_value(Bucket, Suffix) of
						{ok, JSON} ->
							DataKey = << KeyPrefix/binary, "_data" >>,
							walk_array(Tail, Links, [
								{DataKey, jsx:decode(JSON)},
								{Key, Value}|Acc]);
						_ ->
							walk_array(Tail, Links, [{Key, Value}|Acc])
					end
			end;
		_ ->
			walk_array(Tail, Links, [{Key, Value}|Acc])
	end.
