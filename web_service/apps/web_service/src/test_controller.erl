
%% @doc car handler.
-module(test_controller).
-compile(export_all).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"GET">>, <<"OPTION">>], Req, State}.


content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, check}],
		Req, State}.

content_types_provided(Req, State) ->
		{[
			{<<"application/json">>, check}
		], Req, State}.
			
check(Req, State) ->
	{ok, Body, Req2} = cowboy_req:body(Req),
	io:format("~p", [Body]),
	B = <<"{\n\t\"dodo\": 123\n}">>,
	Req3 = cowboy_req:set_resp_body(<<"Hello world">>, Req2),
    {true, Req3, State}.