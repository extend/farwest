%% /farwest/templates/[name]
-module(fw_templates_handler).

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
-export([template_to_html/2]).
-export([template_to_json/2]).
-export([template_from_json/2]).
-export([template_from_form/2]).
%% @todo -export([template_from_json/2]).
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
	{Name, Req2} = cowboy_req:binding(name, Req),
	{ok, Req2, State#state{collection=Name =:= undefined}}.

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
		{<<"text/html">>, template_to_html},
		{<<"application/json">>, template_to_json}
	], Req, State}.

%% Only allow UTF-8.
charsets_provided(Req, State) ->
	{[{<<"utf-8">>, 1000}, {<<"*">>, 0}], Req, State}.

resource_exists(Req, State=#state{collection=true}) ->
	{true, Req, State};
resource_exists(Req, State=#state{collection=false}) ->
	{Name, Req2} = cowboy_req:binding(name, Req),
	case fw_templates_server:get_template(Name) of
		{ok, _} -> {true, Req2, State};
		{error, notfound} -> {false, Req2, State}
	end.

content_types_accepted(Req, State=#state{collection=false}) ->
	{[
		{<<"application/x-www-form-urlencoded;charset=UTF-8">>,
			template_from_form},
		{<<"application/json">>, template_from_json}
	], Req, State}.

is_conflict(Req, State) ->
	case cowboy_req:qs_val(<<"new">>, Req) of
		{undefined, Req2} -> {false, Req2, State};
		{_, Req2} ->
			{Name, Req3} = cowboy_req:binding(name, Req2),
			case fw_templates_server:get_template(Name) of
				{ok, _} -> {true, Req3, State};
				{error, notfound} -> {false, Req3, State}
			end
	end.

%% Generate an ETag based on the compile time of this module.
generate_etag(Req, State=#state{collection=true}) ->
	{undefined, Req, State};
%% @todo This is cool but doesn't take into accounts templates
%% (both rendered and listed).
%%	{time, {Y, Mo, D, H, Mi, S}}
%%		= lists:keyfind(time, 1, code:module_info(compile)),
%%	ETag = base64:encode(<< Y:16, Mo:8, D:8, H:8, Mi:8, S:8 >>),
%%	{<< $", ETag/binary, $" >>, Req, State};
generate_etag(Req, State=#state{collection=false}) ->
	%% @todo ETag
	{undefined, Req, State}.

collection_to_html(Req, State) ->
	{ok, Templates} = fw_templates_server:list_templates(),
	{ok, Body} = fw_templates_dtl:render([
		{templates, lists:sort(Templates)}
	]),
	{Body, Req, State}.

collection_to_json(Req, State) ->
	%% @todo
	Body = <<"{}">>,
	{Body, Req, State}.

template_to_html(Req, State) ->
	{ok, Templates} = fw_templates_server:list_templates(),
	{Name, Req2} = cowboy_req:binding(name, Req),
	{ok, Data} = fw_templates_server:get_template(Name),
	{<<"revision">>, Revision} = lists:keyfind(<<"revision">>, 1, Data),
	{<<"value">>, Contents} = lists:keyfind(<<"value">>, 1, Data),
	{ok, Body} = fw_templates_dtl:render([
		{templates, lists:sort(Templates)},
		{name, Name},
		{revision, Revision},
		{contents, Contents}
	]),
	{Body, Req2, State}.

template_to_json(Req, State) ->
	%% @todo
	Body = <<"{}">>,
	{Body, Req, State}.

template_from_json(Req, State) ->
	%% @todo
	{true, Req, State}.

%% @todo Later just send JSON from form directly.
template_from_form(Req, State=#state{userid=UserID}) ->
	{Name, Req2} = cowboy_req:binding(name, Req),
	{ok, Values, Req3} = cowboy_req:body_qs(infinity, Req2),
	{<<"contents">>, Contents} = lists:keyfind(<<"contents">>, 1, Values),
	{<<"comments">>, Comments} = lists:keyfind(<<"comments">>, 1, Values),
	case fw_templates_server:set_template(
			Name, UserID, Contents, Comments) of
		ok ->
			{true, Req3, State};
		{error, Reason} ->
			ContentsError = list_to_binary(io_lib:format("~p", [Reason])),
			Req4 = cowboy_req:set_resp_body(
				jsx:encode([{contents, ContentsError}]), Req3),
			{false, Req4, State}
	end.

%% @todo template_from_json

delete_resource(Req, State) ->
	%% @todo
	io:format("delete_resource ~p~n", [Req]),
	{true, Req, State}.

delete_completed(Req, State) ->
	%% @todo
	{true, Req, State}.
