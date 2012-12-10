%% /farwest/userfiles/[name]
-module(fw_userfiles_handler).

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
%-export([userfile_to_html/2]).
%-export([userfile_to_json/2]).
-export([allow_missing_post/2]).
-export([post_is_create/2]).
-export([create_path/2]).
-export([userfile_from_multipart/2]).
%% @todo -export([template_from_json/2]).
-export([delete_resource/2]).
-export([delete_completed/2]).

-record(state, {
	auth = undefined,
	collection = undefined :: boolean(),
	subfolder :: binary(),
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
	{[<<"GET">>, <<"POST">>], Req, State};
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
%		{<<"text/html">>, template_to_html},
%		{<<"application/json">>, template_to_json}
	], Req, State}.

%% Only allow UTF-8.
charsets_provided(Req, State) ->
	{[{<<"utf-8">>, 1000}, {<<"*">>, 0}], Req, State}.

resource_exists(Req, State=#state{collection=true}) ->
	{true, Req, State};
resource_exists(Req, State=#state{collection=false}) ->
	{Name, Req2} = cowboy_req:binding(name, Req),
	{fw_userfiles_server:exists(Name), Req2, State}.

is_conflict(Req, State) ->
	case cowboy_req:qs_val(<<"new">>, Req) of
		{undefined, Req2} -> {false, Req2, State};
		{_, Req2} ->
			{Name, Req3} = cowboy_req:binding(name, Req2),
			{fw_userfiles_server:exists(Name), Req3, State}
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
	{ok, Files} = fw_userfiles_server:list_files(),
	{ok, Body} = fw_userfiles_collection_dtl:render([
		{files, lists:sort(Files)}
	]),
	{Body, Req, State}.

collection_to_json(Req, State) ->
	%% @todo
	Body = <<"{}">>,
	{Body, Req, State}.

%template_to_html(Req, State) ->
%	{Name, Req2} = cowboy_req:binding(name, Req),
%	{ok, Data} = fw_templates_server:get_template(Name),
%	{<<"value">>, Contents} = lists:keyfind(<<"value">>, 1, Data),
%	{ok, Body} = fw_templates_editor_dtl:render([
%		{name, Name},
%		{contents, Contents}
%	]),
%	{Body, Req2, State}.
%
%template_to_json(Req, State) ->
%	%% @todo
%	Body = <<"{}">>,
%	{Body, Req, State}.
%
%template_from_json(Req, State) ->
%	%% @todo
%	{true, Req, State}.

allow_missing_post(Req, State) ->
	{true, Req, State}.

post_is_create(Req, State) ->
	{true, Req, State}.

%% @todo Ugly!
create_path(Req, State) ->
	{headers, _, Req2} = cowboy_req:multipart_data(Req),
	{body, Subfolder, Req3} = cowboy_req:multipart_data(Req2),
	{end_of_part, Req4} = cowboy_req:multipart_data(Req3),
	{<< "/farwest/userfiles/", Subfolder/binary >>, Req4,
		State#state{subfolder=Subfolder}}.

content_types_accepted(Req, State) ->
	{[
		%% @todo Allow using multipart/form-data with any param here.
		{'*', userfile_from_multipart}
	], Req, State}.

%% @todo Later just send JSON from form directly? Or use websockets?
userfile_from_multipart(Req, State=#state{subfolder=Subfolder}) ->
	{headers, Headers, Req2} = cowboy_req:multipart_data(Req),
	Filename = multipart_parse_headers(Headers),
	{ok, Data, Req3} = multipart_parse_data(Req2),
	%% @todo ugly
	{headers, _, Req4} = cowboy_req:multipart_data(Req3),
	Comments = case cowboy_req:multipart_data(Req4) of
		{body, C, Req5} ->
			C;
		{end_of_part, Req5} ->
			<<>>
	end,
	%% ok
	ok = fw_userfiles_server:set_file(
		<< Subfolder/binary, $/, Filename/binary >>,
		<<"/users/EspioKaos">>, Data, Comments),
	{true, Req5, State}.

%% @todo Proper parsing.
%% @todo get content-type
multipart_parse_headers([{<<"content-disposition">>, Bin}|_]) ->
	[<<"form-data">>|Parts] = binary:split(Bin, <<";">>, [global, trim]),
	[Ret] = [begin
		$" = binary:last(Filename),
		nomatch = binary:match(Filename, [<< $/ >>, << $\ >>]),
		binary:part(Filename, 0, byte_size(Filename) - 1)
	end || << " filename=\"", Filename/binary >> <- Parts],
	Ret.

multipart_parse_data(Req) ->
	multipart_parse_data(Req, <<>>).
multipart_parse_data(Req, Acc) ->
	case cowboy_req:multipart_data(Req) of
		{body, Chunk, Req2} ->
			multipart_parse_data(Req2, << Acc/binary, Chunk/binary >>);
		{end_of_part, Req2} ->
			{ok, Acc, Req2}
	end.

delete_resource(Req, State) ->
	%% @todo
	io:format("delete_resource ~p~n", [Req]),
	{true, Req, State}.

delete_completed(Req, State) ->
	%% @todo
	{true, Req, State}.
