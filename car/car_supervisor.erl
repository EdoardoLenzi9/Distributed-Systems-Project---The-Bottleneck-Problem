%%%===================================================================
%%% Car supervisor interface
%%%===================================================================

-module(car_supervisor).
-compile(export_all).
-include("car.hrl").

start(Args) -> 
    
    [PName, PSide, PPower, PTurn, PBridgeCapacity, PBridgeLength, PTimeout] = Args,
    {Name, _} = string:to_integer(PName),
    {Side, _} = string:to_integer(PSide),
    {Power, _ } = string:to_integer(PPower),
    {Turn, _ } = string:to_integer(PTurn),
    {BridgeCapacity, _ } = string:to_integer(PBridgeCapacity),
    {BridgeLength, _ } = string:to_integer(PBridgeLength),
    {Timeout, _ } = string:to_integer(PTimeout),
    
    register(Name, self()),    
    Env = utils:load_environment(),

    State = #carState{
                        name = Name, 
                        side = Side, 
                        power = Power, 
                        adj = #adj{frontCars = http_client:get_sync(Name, Side, Power), rearCars = []}, 
                        arrivalTime = utils:get_timestamp(), 
                        state = init,
                        turn = Turn,
                        bridgeCapacity = BridgeCapacity, 
                        bridgeLength = BridgeLength,
                        maxSpeed = Env#env.maxSpeed,
                        tow_truckTime = Env#env.tow_truckTime
                    },

	if Timeout > 0 ->
        utils:log("Launch killer process with timeout")
        %launch killer
    end,
    car:start_link(State),
    loop().


loop() ->
    receive
        stop ->
            car:stop(); 
        sync ->
            car:sync(); 
        crash ->
            car:crash()
    end,
    loop().

killer(Name) ->
    receive
        after 5000 ->
            car:crash(Name)
    end.