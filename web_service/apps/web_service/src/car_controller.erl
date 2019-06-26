
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
	Method = cowboy_req:method(Req),
	{ok, Body, _Req2} = cowboy_req:body(Req),
	{_HTTP, _Domain, Path, _, _Qs} = mochiweb_util:urlsplit(binary_to_list(URL)),
	utils:log("~n~n~p    ~p    ~p~n~n", [Method, Path, Body]),
	ResponseBody = case Path of 
		"/car/sync" ->
			sync_handler(Body)
	end,
	Req3 = cowboy_req:set_resp_body(ResponseBody, Req),
	{true, Req3, State}.


sync_handler(Body) ->
	DecodedTuple = jiffy:decode(Body),
	{[{<<"name">>,Name},{<<"side">>,Side},{<<"power">>,Power}]} = DecodedTuple, 
	jiffy:encode(car_service:add_sync(binary_to_list(Name), binary_to_list(Side), Power)).