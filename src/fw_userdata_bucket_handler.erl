%% /farwest/userdata/bucket
-module(fw_userdata_bucket_handler).

-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([resource_exists/2]).
-export([to_html/2]).
-export([to_json/2]).

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
	{[<<"GET">>], Req, State}.

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

resource_exists(Req, State) ->
	{Bucket, Req2} = cowboy_req:binding(bucket, Req),
	{fw_userdata_server:bucket_exists(Bucket), Req2, State}.

to_html(Req, State) ->
	{ok, Buckets} = fw_userdata_server:list_buckets(),
	{BucketName, Req2} = cowboy_req:binding(bucket, Req),
	{ok, Keys} = fw_userdata_server:get_all_names(BucketName),
	{ok, Body} = fw_userdata_collection_dtl:render([
		{buckets, lists:sort(Buckets)},
		{bucket_name, BucketName},
		{bucket_keys, lists:sort(Keys)}
	]),
	{Body, Req2, State}.

to_json(Req, State) ->
	%% @todo
	Body = <<"{}">>,
	{Body, Req, State}.
