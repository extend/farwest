-module(fw_templates_server).

%% API.
-export([start_link/0]).
-export([stop/0]).
-export([get_template/1]).
-export([get_template/2]).
-export([get_template_history/1]).
-export([list_templates/0]).
-export([set_template/4]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Internal.
-export([compiler_load_template/1]).

-include_lib("kernel/include/file.hrl").

%% Riak bucket name.
-define(BUCKET, <<"fw_templates">>).

-record(state, {
	riak_pid = undefined :: pid()
}).

%% API.

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

get_template(Name) ->
	gen_server:call(?MODULE, {get_template, Name}).

get_template(Name, Revision) ->
	gen_server:call(?MODULE, {get_template, Name, Revision}).

get_template_history(Name) ->
	gen_server:call(?MODULE, {get_template_history, Name}).

list_templates() ->
	gen_server:call(?MODULE, list_templates).

%% @todo Instead of infinity timeout we would like to build templates
%% in a separate process.
set_template(Name, EditBy, Contents, Comments) ->
	gen_server:call(?MODULE, {set_template, Name, EditBy, Contents, Comments},
		infinity).

%% @todo set_deleted(Name, EditBy, Comments)
%% @todo delete(Name)

%% gen_server.

init([]) ->
	{ok, RiakPid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
	ok = init_templates(RiakPid),
	{ok, #state{riak_pid=RiakPid}}.

handle_call(stop, _, State) ->
	{stop, normal, stopped, State};
handle_call({get_template, Name}, _, State=#state{riak_pid=RiakPid}) ->
	{reply, do_get_template(RiakPid, Name), State};
handle_call({get_template, Name, Revision}, _,
		State=#state{riak_pid=RiakPid}) ->
	{reply, do_get_custom_template_revision(RiakPid, Name, Revision), State};
handle_call({get_template_history, Name}, _, State=#state{riak_pid=RiakPid}) ->
	{reply, do_get_template_history(RiakPid, Name), State};
handle_call(list_templates, _, State=#state{riak_pid=RiakPid}) ->
	{reply, do_list_templates(RiakPid), State};
handle_call({set_template, Name, EditBy, Contents, Comments}, _,
		State=#state{riak_pid=RiakPid}) ->
	{reply, do_set_template(RiakPid, Name, EditBy, Contents, Comments), State};
handle_call(_, _, State) ->
	{reply, ignore, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

init_templates(RiakPid) ->
	{ok, Templates} = do_list_templates(RiakPid),
	_ = [begin
		{ok, Template} = do_get_template(RiakPid, Name),
		{<<"value">>, Contents}
			= lists:keyfind(<<"value">>, 1, Template),
		case compile(Name, Contents) of
			{ok, _} -> ok;
			{error, Reason} ->
				lager:error("Compile of template ~p failed with reason: ~p",
					[Name, Reason])
		end
	end || Name <- Templates],
	ok.

do_get_template(RiakPid, Name) ->
	case do_get_custom_template(RiakPid, Name) of
		{ok, Template} -> {ok, Template};
		{error, notfound} ->
			case do_get_farwest_template(Name) of
				{ok, Template} -> {ok, Template};
				{error, _} -> {error, notfound}
			end
	end.

do_get_custom_template(RiakPid, Name) ->
	fw_versioned_buckets:get(RiakPid, ?BUCKET, Name).

%% @todo Probably limit the set of names.
do_get_farwest_template(Name) ->
	Filename = code:lib_dir(farwest) ++ "/templates/"
		++ binary_to_list(Name) ++ ".dtl",
	case file:read_file(Filename) of
		{ok, Contents} ->
			{ok, FileInfo} = file:read_file_info(Filename, [{time, universal}]),
			DateTime = format_date(FileInfo#file_info.mtime),
			{ok, [
				{<<"revision">>, 0},
				{<<"edit_by">>, <<"/farwest/">>},
				{<<"edit_datetime">>, DateTime},
				{<<"contents">>, Contents},
				{<<"comments">>, <<"Default Farwest template">>}
			]};
		{error, Reason} ->
			{error, Reason}
	end.

do_get_custom_template_revision(RiakPid, Name, Revision) ->
	fw_versioned_buckets:get(RiakPid, ?BUCKET, Name, Revision).

do_get_template_history(RiakPid, Name) ->
	fw_versioned_buckets:history(RiakPid, ?BUCKET, Name).

do_list_templates(RiakPid) ->
	fw_versioned_buckets:keys(RiakPid, ?BUCKET).

do_set_template(RiakPid, Name, EditBy, Contents, Comments) ->
	case compile(Name, Contents) of
		{ok, _} ->
			fw_versioned_buckets:set(RiakPid, ?BUCKET, Name,
				EditBy, Contents, Comments);
		{error, Reason} ->
			{error, Reason}
	end.

compile(Name, Bin) ->
	%% @todo Allow configuring some of these options.
	erlydtl:compile(Bin, name_to_module(Name), [
		{custom_filters_modules, [erlydtl_contrib_humanize]},
		{reader, {?MODULE, compiler_load_template}}
	]).

%% @priv
compiler_load_template(Name) ->
	NameBin = list_to_binary(string:substr(Name, 3, length(Name) - 7)),
	%% @todo Have a pool of riakpid we can use.
	{ok, RiakPid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
	{ok, Template} = do_get_template(RiakPid, NameBin),
	ok = riakc_pb_socket:stop(RiakPid),
	{<<"value">>, Contents} = lists:keyfind(<<"value">>, 1, Template),
	{ok, Contents}.

name_to_module(Name) ->
	binary_to_atom(<< Name/binary, "_dtl" >>, latin1).

%% @todo Duplicate of fw_versioned_buckets:format_date/1.
format_date(DateTime) ->
	list_to_binary(dh_date:format("D, d M Y H:i:s \\G\\M\\T", DateTime)).
