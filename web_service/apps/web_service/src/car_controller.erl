
%% @doc car handler.
-module(car_controller).
-compile(export_all).


init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.


allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.


content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, handler}],
		Req, State}.


content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, handler}
	], Req, State}.


handler(Req, State) ->
	URL = cowboy_req:url(Req),
	{ok, Body, Req2} = cowboy_req:body(Req),
	{HTTP, Domain, Path, _, Qs} = mochiweb_util:urlsplit(binary_to_list(URL)),
	ResponseBody = case Path of 
		"/car/sync" ->
			sync_handler(Body)
	end,
	Req3 = cowboy_req:set_resp_body(list_to_binary(ResponseBody), Req),
	{true, Req3, State}.


sync_handler(Body) ->
	DecodedTuple = jiffy:decode(Body),
	{[{<<"name">>,Name},{<<"side">>,Side},{<<"power">>,Power}]} = DecodedTuple, 
	%car_service:addSync(Name, list_to_atom(Side), Power),	
	%Body.
	"Fattoooo".