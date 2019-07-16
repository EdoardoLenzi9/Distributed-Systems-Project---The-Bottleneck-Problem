
%% @doc car handler.
-module(simulation_controller).
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
	{[
		{<<"application/json">>, handler}
	], Req, State}.


%%%===================================================================
%%% end-points
%%%===================================================================

handler(Req, State) ->
	URL = cowboy_req:url(Req),
	Method = cowboy_req:method(Req),
	Body = if Method == <<"POST">> ->
		{ok, ReqBody, _Req2} = cowboy_req:body(Req),
		ReqBody;
	true -> 
		<<"">>
	end,
	{_HTTP, _Domain, Path, _, _Qs} = mochiweb_util:urlsplit(binary_to_list(URL)),
	utils:log("~n~n~p    ~p    ~p~n~n", [Method, Path, Body]),
	ResponseBody = case Path of 
		"/simulation" ->
			state_handler();
		"/simulation/init" ->
			init_handler(Body);
		"/simulation/new" ->
			new_node_handler(Body);
		"/simulation/reset" ->
			reset_handler(Body)
	end,
	Req3 = cowboy_req:set_resp_body(ResponseBody, Req),
	{true, Req3, State}.


state_handler() ->
	jiffy:encode(simulation_service:state()).


init_handler(Body) ->
	DecodedTuple = jiffy:decode(Body),

	{[	{<<"max_speed">>, MaxSpeed},
		{<<"max_RTT">>, MaxRTT},
		{<<"tow_truck_time">>, TowTruckTime},
		{<<"bridge_capacity">>, BridgeCapacity},
		{<<"bridge_length">>, BridgeLength} ]} = DecodedTuple, 
	simulation_service:init(#settingsEntity{ 	max_speed = MaxSpeed,
												max_RTT = MaxRTT,
												tow_truck_time = TowTruckTime,
												bridge_capacity = BridgeCapacity, 
												bridge_length = BridgeLength }),
	jiffy:encode({[{result, success}]}). 


new_node_handler(Body) ->
	DecodedTuple = jiffy:decode(Body),
	{[	{<<"name">>, Name},
		{<<"side">>, Side},
		{<<"power">>, Power},
		{<<"size">>, Size},
		{<<"crash_type">>, CrashType},
		{<<"timeout">>, Timeout} ]} = DecodedTuple, 
		jiffy:encode(simulation_service:new(#newCarEntity{ 	name = binary_to_list(Name), 
															side = Side, 
															power = Power, 
															size = Size,
															crash_type = CrashType,
															timeout = Timeout })).
										

reset_handler(_Body) ->
	simulation_service:reset(),
	jiffy:encode({[{result, success}]}). 