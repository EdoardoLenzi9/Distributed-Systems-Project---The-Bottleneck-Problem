
%% @doc car handler.
-module(car_controller).
-compile(export_all).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, check}],
		Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, check}
	], Req, State}.

check(Req, State) ->
	{ok, Body, Req2} = cowboy_req:body(Req),
	%io:format("~p", [Body]),
	DecodedTuple = jiffy:decode(Body),
	{[{<<"dodo">>,Dodo}]} = DecodedTuple, 
	%Req3 = cowboy_req:set_resp_body(list_to_binary(io:format("Dodo is: ~p", [Dodo])), Req2),
	%ResponseText = atom_to_list(io:format("Dodo is: ~p", [Dodo])),
	Req3 = cowboy_req:set_resp_body(<<"Ciao">>, Req2),
	{true, Req3, State}.
