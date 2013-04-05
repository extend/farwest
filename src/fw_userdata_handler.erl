%% /farwest/userdata/[bucket/key]
-module(fw_userdata_handler).

-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([resource_exists/2]).
-export([content_types_accepted/2]).
-export([is_conflict/2]).
-export([generate_etag/2]).
-export([collection_to_html/2]).
-export([collection_to_json/2]).
-export([userdata_to_html/2]).
-export([userdata_to_json/2]).
-export([userdata_from_form/2]).
-export([delete_resource/2]).
-export([delete_completed/2]).

-record(state, {
	auth = undefined,
	collection = undefined :: boolean(),
	userid = undefined
}).

init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
	State = case lists:keyfind(auth, 1, Opts) of
		{auth, AuthOpts} -> #state{auth=AuthOpts};
		false -> #state{}
	end,
	{Bucket, Req2} = cowboy_req:binding(bucket, Req),
	{Key, Req3} = cowboy_req:path_info(Req2),
	if	Bucket =:= undefined, Key =:= undefined ->
			{ok, Req3, State#state{collection=true}};
		Bucket =/= undefined, Key =/= undefined ->
			{ok, Req3, State#state{collection=false}}
	end.

allowed_methods(Req, State=#state{collection=true}) ->
	{[<<"GET">>], Req, State};
allowed_methods(Req, State=#state{collection=false}) ->
	{[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

is_authorized(Req, State=#state{auth=undefined}) ->
	{true, Req, State};
is_authorized(Req, State=#state{auth=AuthOpts}) ->
	case fw_auth:authenticate(Req, AuthOpts) of
		{false, Req2} ->
			{{false, fw_auth:methods(AuthOpts)}, Req2, State};
		{UserID, Req2} ->
			{true, Req2, State#state{userid=UserID}}
	end.

content_types_provided(Req, State=#state{collection=true}) ->
	{[
		{<<"text/html">>, collection_to_html},
		{<<"application/json">>, collection_to_json}
	], Req, State};
content_types_provided(Req, State=#state{collection=false}) ->
	{[
		{<<"text/html">>, userdata_to_html},
		{<<"application/json">>, userdata_to_json}
	], Req, State}.

%% Only allow UTF-8.
charsets_provided(Req, State) ->
	{[{<<"utf-8">>, 1000}, {<<"*">>, 0}], Req, State}.

resource_exists(Req, State=#state{collection=true}) ->
	{true, Req, State};
resource_exists(Req, State=#state{collection=false}) ->
	{Bucket, Req2} = cowboy_req:binding(bucket, Req),
	{Key, Req3} = cowboy_req:path_info(Req2),
	{fw_userdata_server:exists(Bucket, Key), Req3, State}.

content_types_accepted(Req, State=#state{collection=false}) ->
	{[
		{{<<"application">>,<<"x-www-form-urlencoded">>,[{<<"charset">>,<<"UTF-8">>}]},
			userdata_from_form},
		%% @todo Need normalize CTA for this one
		{<<"application/json">>, template_from_json}
	], Req, State}.

is_conflict(Req, State) ->
	case cowboy_req:qs_val(<<"new">>, Req) of
		{undefined, Req2} -> {false, Req2, State};
		{_, Req2} ->
			{Bucket, Req3} = cowboy_req:binding(bucket, Req2),
			{Key, Req4} = cowboy_req:path_info(Req3),
			{fw_userdata_server:exists(Bucket, Key), Req4, State}
	end.

%% Generate an ETag based on the compile time of this module.
generate_etag(Req, State=#state{collection=true}) ->
	{undefined, Req, State};
%% @todo
%%	{time, {Y, Mo, D, H, Mi, S}}
%%		= lists:keyfind(time, 1, code:module_info(compile)),
%%	ETag = base64:encode(<< Y:16, Mo:8, D:8, H:8, Mi:8, S:8 >>),
%%	{<< $", ETag/binary, $" >>, Req, State};
generate_etag(Req, State=#state{collection=false}) ->
	%% @todo ETag
	{undefined, Req, State}.

collection_to_html(Req, State) ->
	{ok, Buckets} = fw_userdata_server:list_buckets(),
	{ok, Body} = fw_userdata_collection_dtl:render([
		{buckets, lists:sort(Buckets)}
	]),
	{Body, Req, State}.

collection_to_json(Req, State) ->
	%% @todo
	Body = <<"{}">>,
	{Body, Req, State}.

userdata_to_html(Req, State) ->
	{Bucket, Req2} = cowboy_req:binding(bucket, Req),
	{Key, Req3} = cowboy_req:path_info(Req2),
	{ok, Data} = fw_userdata_server:get_data(Bucket, Key),
	{<<"value">>, UserData} = lists:keyfind(<<"value">>, 1, Data),
	{ok, Body} = fw_userdata_editor_dtl:render([
		{bucket, Bucket},
		{key, Key},
		{userdata, UserData}
	]),
	{Body, Req3, State}.

userdata_to_json(Req, State) ->
	%% @todo
	Body = <<"{}">>,
	{Body, Req, State}.

% userdata_from_json(Req, State) ->
% 	%% @todo
% 	{true, Req, State}.

%% @todo Later just send JSON from form directly.
userdata_from_form(Req, State=#state{userid=UserID}) ->
	{Bucket, Req2} = cowboy_req:binding(bucket, Req),
	{Key, Req3} = cowboy_req:path_info(Req2),
	{ok, Values, Req4} = cowboy_req:body_qs(infinity, Req3),
	{<<"userdata">>, UserData} = lists:keyfind(<<"userdata">>, 1, Values),
	{<<"comments">>, Comments} = lists:keyfind(<<"comments">>, 1, Values),
	case fw_userdata_server:set_data(Bucket, Key,
			UserID, UserData, Comments) of
		ok ->
			{true, Req4, State};
		{error, Reason} ->
			io:format("put error ~s~n", [Reason]),
			Req5 = cowboy_req:set_resp_body(
				jsx:encode([{userdata, erlang:atom_to_binary(Reason, latin1)}]),
				Req4),
			{false, Req5, State}
	end.

%% @todo put_template_from_json

delete_resource(Req, State) ->
	%% @todo
	io:format("delete_resource ~p~n", [Req]),
	{true, Req, State}.

delete_completed(Req, State) ->
	%% @todo
	{true, Req, State}.
