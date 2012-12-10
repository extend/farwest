-module(fw_userfiles_server).

%% API.
-export([start_link/0]).
-export([stop/0]).
-export([exists/1]).
-export([list_files/0]).
-export([set_file/4]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Riak bucket prefix.
-define(BUCKET, <<"fw_userfiles">>).

-record(state, {
	riak_pid = undefined :: pid()
}).

%% API.

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

exists(Filename) ->
	gen_server:call(?MODULE, {exists, Filename}).

list_files() ->
	gen_server:call(?MODULE, list_files).

set_file(Filename, EditBy, UserFile, Comments) ->
	gen_server:call(?MODULE, {set_file, Filename, EditBy, UserFile, Comments}).

%% gen_server.

init([]) ->
	{ok, RiakPid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
	{ok, #state{riak_pid=RiakPid}}.

handle_call(stop, _, State) ->
	{stop, normal, stopped, State};
handle_call({exists, Filename}, _, State=#state{riak_pid=RiakPid}) ->
	{reply, do_exists(RiakPid, Filename), State};
handle_call(list_files, _, State=#state{riak_pid=RiakPid}) ->
	{reply, do_list_files(RiakPid), State};
handle_call({set_file, Filename, EditBy, UserFile, Comments}, _,
		State=#state{riak_pid=RiakPid}) ->
	{reply, do_set_file(RiakPid, Filename, EditBy, UserFile, Comments), State};
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

do_exists(RiakPid, Filename) ->
	fw_versioned_buckets:exists(RiakPid, ?BUCKET, Filename).

do_list_files(RiakPid) ->
	fw_versioned_buckets:keys(RiakPid, ?BUCKET).

do_set_file(RiakPid, Filename, EditBy, UserFile, Comments) ->
	try
		{ok, AbsFilename} = get_abs_filename(Filename),
		ok = filelib:ensure_dir(AbsFilename),
		ok = file:write_file(AbsFilename, UserFile),
		fw_versioned_buckets:set(RiakPid, ?BUCKET, Filename,
			EditBy, base64:encode(UserFile), Comments)
	catch _:Reason ->
		{error, Reason}
	end.

get_abs_filename(Filename) ->
	{ok, BasePath} = application:get_env(farwest, userfiles_dir),
	BasePathBin = list_to_binary(BasePath),
	{ok, << BasePathBin/binary, $/, Filename/binary >>}.
