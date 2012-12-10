-module(fw_versioned_buckets).

%% API.
%% @todo -export([delete/3]).
-export([exists/3]).
-export([get/3]).
-export([get/4]).
-export([get_all/2]).
-export([history/3]).
-export([keys/2]).
-export([set/6]).

%% Data structure version.
-define(VERSION, 1).

%% API.

exists(RiakPid, Bucket, Key) ->
	case history(RiakPid, Bucket, Key) of
		{ok, _} -> true;
		{error, notfound} -> false
	end.

get(RiakPid, Bucket, Key) ->
	case history(RiakPid, Bucket, Key) of
		{ok, History} -> {ok, hd(History)};
		Error -> Error
	end.

get(RiakPid, Bucket, Key, Revision) ->
	case history(RiakPid, Bucket, Key) of
		{ok, History} -> find_revision(History, Revision);
		Error -> Error
	end.

get_all(RiakPid, Bucket) ->
	{ok, Keys} = keys(RiakPid, Bucket),
	{ok, [begin
		{ok, Data} = get(RiakPid, Bucket, Key),
		{Key, Data}
	end || Key <- Keys]}.

history(RiakPid, Bucket, Key) ->
	case riakc_pb_socket:get(RiakPid, Bucket, Key) of
		{ok, Object} ->
			JSON = riakc_obj:get_value(Object),
			Data = jsx:decode(JSON),
			{<<"history">>, History} = lists:keyfind(<<"history">>, 1, Data),
			{ok, History};
		{error, notfound} ->
			{error, notfound}
	end.

keys(RiakPid, Bucket) ->
	riakc_pb_socket:list_keys(RiakPid, Bucket).

set(RiakPid, Bucket, Key, EditBy, Value, Comments) ->
	EditDateTime = format_date(calendar:universal_time()),
	case riakc_pb_socket:get(RiakPid, Bucket, Key) of
		{ok, Object} ->
			JSON = riakc_obj:get_value(Object),
			Data = jsx:decode(JSON),
			{<<"next_revision">>, Revision}
				= lists:keyfind(<<"next_revision">>, 1, Data),
			{<<"history">>, History}
				= lists:keyfind(<<"history">>, 1, Data),
			Edit = [
				{<<"revision">>, Revision},
				{<<"edit_by">>, EditBy},
				{<<"edit_datetime">>, EditDateTime},
				{<<"value">>, Value},
				{<<"comments">>, Comments}
			],
			Data2 = lists:keyreplace(<<"next_revision">>, 1, Data,
				{<<"next_revision">>, Revision + 1}),
			Data3 = lists:keyreplace(<<"history">>, 1, Data2,
				{<<"history">>, [Edit|History]}),
			JSON2 = jsx:encode(Data3),
			Object2 = riakc_obj:update_value(Object, JSON2),
			riakc_pb_socket:put(RiakPid, Object2),
			ok;
		{error, notfound} ->
			Data = [
				{<<"version">>, ?VERSION},
				{<<"next_revision">>, 2},
				{<<"history">>, [[
					{<<"revision">>, 1},
					{<<"edit_by">>, EditBy},
					{<<"edit_datetime">>, EditDateTime},
					{<<"value">>, Value},
					{<<"comments">>, Comments}
				]]}
			],
			JSON = jsx:encode(Data),
			Object = riakc_obj:new(Bucket, Key, JSON, "application/json"),
			riakc_pb_socket:put(RiakPid, Object),
			ok
	end.

%% Internal.

find_revision([], _) ->
	{error, notfound};
find_revision([Data|Tail], Revision) ->
	case lists:keyfind(<<"revision">>, 1, Data) of
		{<<"revision">>, Revision} ->
			{ok, Data};
		_ ->
			find_revision(Tail, Revision)
	end.

format_date(DateTime) ->
	list_to_binary(dh_date:format("D, d M Y H:i:s \\G\\M\\T", DateTime)).
