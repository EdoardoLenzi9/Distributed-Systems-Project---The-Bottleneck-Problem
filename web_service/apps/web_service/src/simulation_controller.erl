
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
	%utils:log("~n~n~p    ~p    ~p~n~n", [Method, Path, Body]),
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

	{[	
		{<<"process_visibility">>, ProcessVisibility},
		{<<"max_speed">>, MaxSpeed},
		{<<"max_RTT">>, MaxRTT},
		{<<"tow_truck_time">>, TowTruckTime},
		{<<"bridge_capacity">>, BridgeCapacity},
		{<<"bridge_length">>, BridgeLength} ]} = DecodedTuple, 
	utils:log("~n~p~n", [TowTruckTime]),
	simulation_service:init(#settings_entity{ 	process_visibility = list_to_atom(binary_to_list(ProcessVisibility)),
												max_speed = bin_to_int(MaxSpeed),
												max_RTT = bin_to_int(MaxRTT),
												tow_truck_time = bin_to_int(TowTruckTime),
												bridge_capacity = bin_to_int(BridgeCapacity), 
												bridge_length = bin_to_int(BridgeLength) }),
	jiffy:encode({[{result, success}]}). 


bin_to_int(Bin) ->
	list_to_integer(binary_to_list(Bin)).


new_node_handler(Body) ->
	DecodedTuple = jiffy:decode(Body),
	{[	{<<"host">>, Host},
		{<<"port">>, Port},
		{<<"name">>, Name},
		{<<"side">>, Side},
		{<<"power">>, Power},
		{<<"size">>, Size},
		{<<"crash_type">>, CrashType},
		{<<"timeout">>, Timeout} ]} = DecodedTuple, 
		jiffy:encode(simulation_service:new(#new_car_entity{ 	host = binary_to_list(Host),
															port = binary_to_list(Port),
															name = binary_to_list(Name), 
															side = Side, 
															power = Power, 
															size = Size,
															crash_type = CrashType,
															timeout = Timeout })).
										

reset_handler(_Body) ->
	simulation_service:reset(),
	jiffy:encode({[{result, success}]}). 