-module(fw_ssl_auth_method).

-export([name/1]).
-export([authenticate/3]).
-export([certfile_to_issuer_id/1]).
-export([socket_to_issuer_id/1]).

name(_Opts) ->
	<<"public-key">>.

authenticate(Req, Handler, Opts) ->
	[Socket, Transport] = cowboy_req:get([socket, transport], Req),
	case Transport:name() of
		ssl ->
			case socket_to_issuer_id(Socket) of
				false ->
					{false, Req};
				IssuerID ->
					{Handler:authenticate(IssuerID, Opts), Req}
			end;
		_ ->
			{false, Req}
	end.

certfile_to_issuer_id(Filename) ->
	{ok, Data} = file:read_file(Filename),
	[{'Certificate', Cert, not_encrypted}] = public_key:pem_decode(Data),
	{ok, IssuerID} = public_key:pkix_issuer_id(Cert, self),
	IssuerID.

socket_to_issuer_id(Socket) ->
	case ssl:peercert(Socket) of
		{error, no_peercert} ->
			false;
		{ok, Cert} ->
			{ok, IssuerID} = public_key:pkix_issuer_id(Cert, self),
			IssuerID
	end.
