
%% @doc car handler.
-module(car_controller).
-compile(export_all).
-include("entity.hrl").

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.


allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.


content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, handler}],
		Req, State}.


content_types_provided(Req, State) ->
	{[{<<"application/json">>, handler}
	], Req, State}.


%%%===================================================================
%%% end-points
%%%===================================================================

handler(Req, State) ->
	URL = cowboy_req:url(Req),
	Method = cowboy_req:method(Req),
	{ok, Body, _Req2} = cowboy_req:body(Req),
	{_HTTP, _Domain, Path, _, _Qs} = mochiweb_util:urlsplit(binary_to_list(URL)),
	utils:log("~n~n~p    ~p    ~p~n~n", [Method, Path, Body]),
	ResponseBody = case Path of 
		"/car/sync" ->
			sync_handler(Body);
		"/car/adj" ->
			adj_handler(Body);
		"/car/adj/last" ->
			last_adj_handler(Body);
		"/car/kill" ->
			kill_handler(Body)
	end,
	Req3 = cowboy_req:set_resp_body(ResponseBody, Req),
	{true, Req3, State}.


sync_handler(Body) ->
	DecodedTuple = jiffy:decode(Body),
	{[	{<<"name">>, Name},
		{<<"side">>, Side},
		{<<"power">>, Power} ]} = DecodedTuple, 
		jiffy:encode(car_service:sync(#sync_entity{	name = list_to_atom(binary_to_list(Name)), 
													side = Side, 
													power = Power
												  })).

adj_handler(Body) ->
	DecodedTuple = jiffy:decode(Body),
	{[	{<<"host">>, Host},
		{<<"ip">>, Ip},
		{<<"name">>, Name},
		{<<"side">>, Side},
		{<<"power">>, Power},
		{<<"size">>, Size},
		{<<"position">>, Position},
		{<<"crossing">>, Crossing},
		{<<"arrival_time">>, ArrivalTime},
		{<<"delta">>, Delta},
		{<<"state">>, State},
		{<<"crash_type">>, CrashType} ]} = DecodedTuple, 
	jiffy:encode(car_service:adj(#adj_entity{ 	
												name = list_to_atom(binary_to_list(Name)), 
												host = list_to_atom(binary_to_list(Host)), 
												ip = list_to_atom(binary_to_list(Ip)), 
											 	side = Side, 
											 	power = Power, 
												size = Size, 
												position = Position, 
												crossing = Crossing, 
											 	arrival_time = ArrivalTime, 
												delta = Delta, 
												state = list_to_atom(binary_to_list(State)),
												crash_type = CrashType })).


last_adj_handler(Body) ->
	DecodedTuple = jiffy:decode(Body),
	{[	{<<"side">>, Side} ]} = DecodedTuple, 
	jiffy:encode(car_service:last_adj(Side)).										


kill_handler(Body) ->
	DecodedTuple = jiffy:decode(Body),
	{[	{<<"name">>, Name},
		{<<"target">>, Target} ]} = DecodedTuple, 
		jiffy:encode( car_service:kill( 
										list_to_atom(binary_to_list( Name ) ), 
										list_to_atom(binary_to_list( Target ) )
									  ) ).