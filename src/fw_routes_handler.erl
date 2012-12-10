%% /farwest/templates/[name]
-module(fw_routes_handler).

-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([resource_exists/2]).
-export([content_types_accepted/2]).
-export([is_conflict/2]).
-export([routes_to_html/2]).
-export([routes_to_json/2]).
-export([routes_from_json/2]).
-export([routes_from_form/2]).

-record(state, {
	auth = undefined,
	userid = undefined
}).

init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
	State = case lists:keyfind(auth, 1, Opts) of
		{auth, AuthOpts} -> #state{auth=AuthOpts};
		false -> #state{}
	end,
	{ok, Req, State}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"PUT">>], Req, State}.

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
		{<<"text/html">>, routes_to_html},
		{<<"application/json">>, routes_to_json}
	], Req, State}.

%% Only allow UTF-8.
charsets_provided(Req, State) ->
	{[{<<"utf-8">>, 1000}, {<<"*">>, 0}], Req, State}.

resource_exists(Req, State) ->
	{true, Req, State}.

content_types_accepted(Req, State) ->
	{[
		{<<"application/x-www-form-urlencoded;charset=UTF-8">>,
			routes_from_form},
		{<<"application/json">>, routes_from_json}
	], Req, State}.

is_conflict(Req, State) ->
	%% @todo Should probably handle that.
	{false, Req, State}.

routes_to_html(Req, State) ->
	{ok, RoutesFile} = application:get_env(farwest, routes_file_path),
	{ok, Contents} = file:read_file(RoutesFile),
	{ok, Body} = fw_routes_dtl:render([
		{contents, Contents}
	]),
	{Body, Req, State}.

routes_to_json(Req, State) ->
	%% @todo
	Body = <<"{}">>,
	{Body, Req, State}.

routes_from_json(Req, State) ->
	%% @todo
	{true, Req, State}.

%% @todo Later just send JSON from form directly.
routes_from_form(Req, State) ->
	{ok, Values, Req2} = cowboy_req:body_qs(Req),
	{<<"contents">>, Contents} = lists:keyfind(<<"contents">>, 1, Values),
	case try_compile(Contents) of
		{ok, Dispatch} ->
			ok = cowboy:set_env(farwest_http, dispatch, Dispatch),
			ok = cowboy:set_env(farwest_https, dispatch, Dispatch),
			{ok, RoutesFile} = application:get_env(farwest, routes_file_path),
			ok = file:write_file(RoutesFile, Contents),
			{true, Req2, State};
		{error, Reason} ->
			ContentsError = list_to_binary(io_lib:format("~p", [Reason])),
			Req3 = cowboy_req:set_resp_body(
				jsx:encode([{contents, ContentsError}]), Req2),
			{false, Req3, State}
	end.

try_compile(Routes) ->
	TmpFilename = "/tmp/" ++ integer_to_list(erlang:phash2(make_ref())),
	ok = file:write_file(TmpFilename, Routes),
	Ret = case file:consult(TmpFilename) of
		{ok, Routes2} ->
			try cowboy_router:compile(Routes2) of
				Dispatch ->
					{ok, Dispatch}
			catch _:_ ->
				{error, "Compilation failed."}
			end;
		Error ->
			Error
	end,
	ok = file:delete(TmpFilename),
	Ret.
