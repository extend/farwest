-module(fw_userdata_server).

%% API.
-export([start_link/0]).
-export([stop/0]).
-export([bucket_exists/1]).
-export([exists/2]).
-export([list_buckets/0]).
-export([get_all_names/1]).
-export([get_all_values/1]).
-export([get_data/2]).
-export([get_value/2]).
%% @todo get a specific data revision
%% @todo get the data revision history
-export([set_data/5]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Riak bucket prefix.
-define(PREFIX, "fw_userdata_").

-record(state, {
	riak_pid = undefined :: pid()
}).

%% API.

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

bucket_exists(Bucket) ->
	gen_server:call(?MODULE, {bucket_exists, Bucket}).

exists(Bucket, Key) ->
	gen_server:call(?MODULE, {exists, Bucket, Key}).

list_buckets() ->
	gen_server:call(?MODULE, list_buckets).

get_all_names(Bucket) ->
	gen_server:call(?MODULE, {get_all_names, Bucket}).

get_all_values(Bucket) ->
	gen_server:call(?MODULE, {get_all_values, Bucket}).

get_data(Bucket, Key) ->
	gen_server:call(?MODULE, {get_data, Bucket, Key}).

get_value(Bucket, Key) ->
	gen_server:call(?MODULE, {get_value, Bucket, Key}).

set_data(Bucket, Key, EditBy, UserData, Comments) ->
	gen_server:call(?MODULE, {set_data, Bucket, Key,
		EditBy, UserData, Comments}).

%% gen_server.

init([]) ->
	{ok, RiakPid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
	{ok, #state{riak_pid=RiakPid}}.

handle_call(stop, _, State) ->
	{stop, normal, stopped, State};
handle_call({bucket_exists, Bucket}, _, State=#state{riak_pid=RiakPid}) ->
	{ok, List} = do_list_buckets(RiakPid),
	{reply, lists:member(Bucket, List), State};
handle_call({exists, Bucket, Key}, _, State=#state{riak_pid=RiakPid}) ->
	{reply, do_exists(RiakPid, Bucket, Key), State};
handle_call(list_buckets, _, State=#state{riak_pid=RiakPid}) ->
	{reply, do_list_buckets(RiakPid), State};
handle_call({get_all_names, Bucket}, _, State=#state{riak_pid=RiakPid}) ->
	{reply, do_get_all_names(RiakPid, Bucket), State};
handle_call({get_all_values, Bucket}, _, State=#state{riak_pid=RiakPid}) ->
	{reply, do_get_all_values(RiakPid, Bucket), State};
handle_call({get_data, Bucket, Key}, _, State=#state{riak_pid=RiakPid}) ->
	{reply, do_get_data(RiakPid, Bucket, Key), State};
handle_call({get_value, Bucket, Key}, _, State=#state{riak_pid=RiakPid}) ->
	{reply, do_get_value(RiakPid, Bucket, Key), State};
handle_call({set_data, Bucket, Key, EditBy, UserData, Comments}, _,
		State=#state{riak_pid=RiakPid}) ->
	{reply, do_set_data(RiakPid, Bucket, Key,
		EditBy, UserData, Comments), State};
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

do_exists(RiakPid, Bucket = << ?PREFIX, Suffix/binary >>, Key)
		when byte_size(Suffix) > 0 ->
	fw_versioned_buckets:exists(RiakPid, Bucket, Key).

do_list_buckets(RiakPid) ->
	{ok, Buckets} = riakc_pb_socket:list_buckets(RiakPid),
	Buckets2 = [B || B = << ?PREFIX, _/binary >> <- Buckets],
	{ok, Buckets2}.

do_get_all_names(RiakPid, Bucket = << ?PREFIX, Suffix/binary >>)
		when byte_size(Suffix) > 0 ->
	fw_versioned_buckets:keys(RiakPid, Bucket).

do_get_all_values(RiakPid, Bucket = << ?PREFIX, Suffix/binary >>)
		when byte_size(Suffix) > 0 ->
	{ok, All} = fw_versioned_buckets:get_all(RiakPid, Bucket),
	ValuesList = [begin
		{<<"value">>, Value} = lists:keyfind(<<"value">>, 1, Data),
		jsx:decode(Value)
	end || {_, Data} <- All],
	{ok, ValuesList}.

do_get_data(RiakPid, Bucket = << ?PREFIX, Suffix/binary >>, Key)
		when byte_size(Suffix) > 0 ->
	fw_versioned_buckets:get(RiakPid, Bucket, Key).

do_get_value(RiakPid, Bucket = << ?PREFIX, Suffix/binary >>, Key)
		when byte_size(Suffix) > 0 ->
	case fw_versioned_buckets:get(RiakPid, Bucket, Key) of
		{ok, Data} ->
			{<<"value">>, Value} = lists:keyfind(<<"value">>, 1, Data),
			{ok, Value};
		E = {error, _} ->
			E
	end.

do_set_data(RiakPid, Bucket = << ?PREFIX, Suffix/binary >>, Key,
		EditBy, UserData, Comments) when byte_size(Suffix) > 0 ->
	try jsx:decode(UserData) of _ ->
		fw_versioned_buckets:set(RiakPid, Bucket, Key,
			EditBy, UserData, Comments)
	catch _:Reason ->
		{error, Reason}
	end.
