-module(fw_hooks).

-export([onresponse/4]).

onresponse(Status, _, _, Req) ->
	NameStr = "fw_onresponse_" ++ integer_to_list(Status) ++ "_dtl",
	try
		Module = list_to_existing_atom(NameStr),
		{ok, Body} = Module:render([]),
		{ok, Req2} = cowboy_req:reply(Status,
			[{<<"content-type">>, <<"text/html">>}],
			Body, Req),
		Req2
	catch _:_ ->
		%% The template for this error didn't exist.
		Req
	end.
