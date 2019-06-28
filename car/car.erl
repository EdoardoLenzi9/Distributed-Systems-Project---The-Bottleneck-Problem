-module(car).
-behaviour(gen_statem).

-include("car.hrl"). 
-include("car_std.hrl").
-include("car_api.hrl").


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================


init([Name, Side, Power, Turn, BridgeCapacity, BridgeCrossingTime, Timeout]) ->
    utils:log("STATE Init - Broken car, Timeout:~p", [Timeout]),
    flow:launchEvent(killer, [Name, Timeout]),
    flow:launchEvent(launcher, [Name, defaultBehaviour]),
    utils:log("STATE TRANSITION: Init -> Create"),
    {ok, create, #carState{
                            name = Name, 
                            side = Side, 
                            power = Power, 
                            adj = #adj{frontCars = getSyncAdj(Name, Side, Power), rearCars = []}, 
                            arrivalTime = utils:getTimeStamp(), 
                            turn = Turn,
                            bridgeCapacity = BridgeCapacity, 
                            bridgeCrossingTime = BridgeCrossingTime
                        }};
init([Name, Side, Power, Turn, BridgeCapacity, BridgeCrossingTime]) ->
    utils:log("STATE Init"),
    flow:launchEvent(launcher, [Name, defaultBehaviour]),
    utils:log("STATE TRANSITION: Init -> Create"),
    {ok, create, #carState{
                            name = Name, 
                            side = Side, 
                            power = Power, 
                            adj = #adj{frontCars = getSyncAdj(Name, Side, Power), rearCars = []}, 
                            arrivalTime = utils:getTimeStamp(), 
                            turn = Turn,
                            bridgeCapacity = BridgeCapacity, 
                            bridgeCrossingTime = BridgeCrossingTime
                        }}.
        

create({call, From}, Event, Data) ->
    utils:log("STATE Create"),
    case Event of        
        engineCrash -> 
            flow:launchEvent(towTruck, [2000]),
            flow:next(dead, Data, From);
        systemCrash -> 
            flow:next(dead, Data, From);
        check ->
            flow:keep(Data, From, ok);
        {propagateFront, Event, Counter} -> 
            message:propagateFrontHandler(Event, Counter, Data);
        {propagateRear, Event, Counter} -> 
            message:propagateRearHandler(Event, Counter, Data);
        sync ->
            utils:log("STATE Create - Event sync"),
            no_sync;
        defaultBehaviour ->
            utils:log("STATE Create - Event defaultBehaviour"),
            flow:callTowTruck(Data),
            if Data#carState.adj#adj.frontCars =/= [] ->
                utils:log("Syncronize with front car"),
                Pivot = utils:lastElement(Data#carState.adj#adj.frontCars),
                flow:next(queue, updateDelta(Data, utils:berkeley(Pivot)), From);
            true -> 
                utils:log("The only car - no sync needed"),
                flow:next(leader, Data, From)
            end
    end.


queue({call, From}, Event, Data) ->
    case Event of
        crash ->
            flow:next(dead, Data, From);
        {propagateFront, Event, Counter} -> 
            message:propagateFrontHandler(Event, Counter, Data);
        {propagateRear, Event, Counter} -> 
            message:propagateRearHandler(Event, Counter, Data);
        {readAndPropagateFront, Event, Counter} -> 
            message:readAndPropagateFrontHandler(Event, Counter, Data);
        {readAndPropagateRear, Event, Counter} -> 
            message:readAndPropagateRearHandler(Event, Counter, Data);
        defaultBehaviour ->
            utils:log("STATE Create - Event defaultBehaviour"),
            flow:callTowTruck(Data);
        {crossing, _WaitingCar} ->
            flow:next(crossing, Data, From);
        {newCar, front, NewCar} -> 
            flow:keep(updateAdj(Data, Data#carState.adj#adj.frontCars ++ [NewCar]), From);
        {newCar, rear, NewCar} -> 
            flow:keep(updateAdj(Data, [NewCar | Data#carState.adj#adj.frontCars]), From);
	    leader ->
            utils:log("STATE Queue - Event leader"),
            flow:next(leader, Data, From)
    end.


leader({call, From}, Event, Data) ->
    case Event of
        crash ->
            flow:next(dead, Data, From);
        {propagateFront, Event, Counter} -> 
            message:propagateFrontHandler(Event, Counter, Data),
            flow:keep(Data, From);
        {propagateRear, Event, Counter} -> 
            message:propagateRearHandler(Event, Counter, Data),
            flow:keep(Data, From);
        {readAndPropagateFront, Event, Counter} -> 
            message:readAndPropagateFrontHandler(Event, Counter, Data),
            flow:keep(Data, From);
        {readAndPropagateRear, Event, Counter} -> 
            message:readAndPropagateRearHandler(Event, Counter, Data),
            flow:keep(Data, From);
        {crossing, _WaitingCar} ->
            flow:next(crossing, Data, From);
        {newCar, front, NewCar} -> 
            flow:keep(updateAdj(Data, Data#carState.adj#adj.frontCars ++ [NewCar]), From);
        {newCar, rear, NewCar} -> 
            flow:keep(updateAdj(Data, [NewCar | Data#carState.adj#adj.frontCars]), From);
        defaultBehaviour ->
            utils:log("STATE Leader - Event defaultBehaviour"),
            flow:callTowTruck(Data),
            WaitingCar = utils:lastElement(Data#carState.adj#adj.frontCars),
            message:readAndPropagateRearHandler({crossing, WaitingCar}, Data#carState.bridgeCapacity, Data),
            flow:keep(Data, From)
        end.


crossing({call, From}, Event, Data) ->
    case Event of
    crash ->
        flow:next(dead, Data, From);
    {propagateFront, Event, Counter} -> 
        message:propagateFrontHandler(Event, Counter, Data);
    {propagateRear, Event, Counter} -> 
        message:propagateRearHandler(Event, Counter, Data);
    {readAndPropagateFront, Event, Counter} -> 
        message:readAndPropagateFrontHandler(Event, Counter, Data);
    {readAndPropagateRear, Event, Counter} -> 
        message:readAndPropagateRearHandler(Event, Counter, Data);
    defaultBehaviour ->
        utils:log("STATE Leader - Event defaultBehaviour"),
        %callTowTruck(Data)
        flow:keep(Data, From)
    end.


dead({call, _From}, Event, Data) -> 
    case Event of
        removed ->
            % notifica agli altri
            stop(Data#carState.name)
    end.