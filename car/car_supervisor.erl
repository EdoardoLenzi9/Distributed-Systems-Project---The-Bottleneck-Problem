%%%===================================================================
%%% Car supervisor interface
%%%===================================================================

-module(car_supervisor).
-compile(export_all).


start(Args) -> 
    [PName, PSide, PPower, PTurn, PBridgeCapacity, PBridgeCrossingTime, PTimeout] = Args,
    Name = list_to_atom(PName),
    Side = list_to_atom(PSide),
    {Power, _ } = string:to_integer(PPower),
    {Turn, _ } = string:to_integer(PTurn),
    {BridgeCapacity, _ } = string:to_integer(PBridgeCapacity),
    {BridgeCrossingTime, _ } = string:to_integer(PBridgeCrossingTime),
    {Timeout, _ } = string:to_integer(PTimeout),

    register(Name, self()),    

	if Timeout > 0 ->
		car:start_link(Name, Side, Power, Turn, BridgeCapacity, BridgeCrossingTime, Timeout);
	true ->
		car:start_link(Name, Side, Power, Turn, BridgeCapacity, BridgeCrossingTime)
    end,
    loop().


loop() ->
    receive
        stop ->
            car:stop(); 
        sync ->
            car:sync(); 
        crash ->
            car:crash(); 
        newleader ->
            car:newleader()
    end,
    loop().