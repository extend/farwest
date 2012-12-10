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

start(_, _) ->
	Port = int_env(http_port, 8080),
	SSLPort = int_env(https_port, 8443),
	Certfile = path_env(https_cert),
	CACertfile = path_env(https_cacert),
	{ok, Routes} = file:consult(path_env(routes_file)),
	%% HTTP.
	{ok, _} = cowboy:start_http(farwest_http, 100,
		[{port, Port}],
		[{dispatch, Routes}, {onresponse, fun fw_hooks:onresponse/4}]
	),
	lager:info("Farwest listening on port ~p~n", [Port]),
	{ok, _} = cowboy:start_https(farwest_https, 100,
		[{port, SSLPort}, {certfile, Certfile},
			{cacertfile, CACertfile}, {verify, verify_peer}],
		[{dispatch, Routes}, {onresponse, fun fw_hooks:onresponse/4}]
	),
	lager:info("Farwest securely listening on port ~p~n", [SSLPort]),
	farwest_sup:start_link().

stop(_) ->
	ok.

%% Internal.

int_env(Key, Default) ->
	case application:get_env(farwest, Key) of
		{ok, Value} when is_integer(Value) ->
			Value;
		undefined ->
			Default
	end.

path_env(Key) ->
	case application:get_env(farwest, Key) of
		{ok, {priv_dir, App, Path}} ->
			code:priv_dir(App) ++ "/" ++ Path;
		{ok, Path} ->
			Path
	end.
