-module(fw_auth).

-export([authenticate/2]).
-export([methods/1]).

authenticate(Req, undefined) ->
	{false, Req};
authenticate(Req, {Methods, Handler, Opts}) ->
	try_auth_methods(Methods, Req, Handler, Opts).

try_auth_methods([], Req, _, _) ->
	{false, Req};
try_auth_methods([Module|Tail], Req, Handler, Opts) ->
	case Module:authenticate(Req, Handler, Opts) of
		{false, Req2} ->
			try_auth_methods(Tail, Req2, Handler, Opts);
		Authenticated ->
			Authenticated
	end.

methods({Methods, _, Opts}) ->
	[$,|Str] = lists:flatten([[$,, Module:name(Opts)] || Module <- Methods]),
	Str.
