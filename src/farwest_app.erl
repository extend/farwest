%% Copyright (c) 2012, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @private
-module(farwest_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

%% The priv_dir environment setting is mandatory.
start(_, _) ->
	farwest_sup:start_link().

start_phase(listen, _, _) ->
	PrivDir = case application:get_env(farwest, priv_dir) of
		{ok, PD} -> PD
	end,
	{ok, Dispatch} = file:consult(PrivDir ++ "/dispatch.conf"),
	{ok, Port} = application:get_env(farwest, port),
	{ok, _} = cowboy:start_listener(farwest, 100,
		cowboy_tcp_transport, [{port, Port}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
	lager:info("Farwest listening on port ~p~n", [Port]),
	ok.

stop(_) ->
	ok.
