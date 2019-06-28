-module(car).
-behaviour(gen_statem).

-include("car.hrl"). 
-include("car_std.hrl").
-include("car_api.hrl").


%%%===================================================================
%%% gen_statem states
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
                            adj = #adj{frontCars = http_client:getSyncAdj(Name, Side, Power), rearCars = []}, 
                            arrivalTime = utils:getTimeStamp(), 
                            state = create,
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
                            adj = #adj{frontCars = http_client:getSyncAdj(Name, Side, Power), rearCars = []}, 
                            arrivalTime = utils:getTimeStamp(), 
                            state = create,
                            turn = Turn,
                            bridgeCapacity = BridgeCapacity, 
                            bridgeCrossingTime = BridgeCrossingTime
                        }}.
        

create({call, From}, Event, Data) ->
    utils:log("STATE Create"),
    case Event of        
        engineCrash -> 
            utils:log("Event engineCrash"),
            flow:launchEvent(towTruck, [Data#carState.name, 2000]),
            flow:next(dead, Data, From);
        systemCrash -> 
            utils:log("Event systemCrash"),
            flow:next(dead, Data, From);
        check ->
            utils:log("Event check"),
            flow:keep(Data, From, ok);
        {propagateFront, Event, Counter} -> 
            utils:log("Event propagateFront"),
            message:propagateFrontHandler(Event, Counter, Data);
        {propagateRear, Event, Counter} -> 
            utils:log("Event propagateRear"),
            message:propagateRearHandler(Event, Counter, Data);
        sync ->
            utils:log("Event sync"),
            no_sync;
        defaultBehaviour ->
            utils:log("Event defaultBehaviour"),
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
    utils:log("STATE Queue"),
    case Event of
        engineCrash -> 
            utils:log("Event engineCrash"),
            flow:launchEvent(towTruck, [Data#carState.name, 2000]),
            flow:next(dead, Data, From);
        systemCrash -> 
            utils:log("Event systemCrash"),
            flow:next(dead, Data, From);
        check ->
            utils:log("Event check"),
            flow:keep(Data, From, ok);
        {propagateFront, Event, Counter} -> 
            utils:log("Event propagateFront"),
            message:propagateFrontHandler(Event, Counter, Data);
        {propagateRear, Event, Counter} -> 
            utils:log("Event propagateRear"),
            message:propagateRearHandler(Event, Counter, Data);
        {readAndPropagateFront, Event, Counter} -> 
            utils:log("Event readAndPropagateFront"),
            message:readAndPropagateFrontHandler(Event, Counter, Data);
        {readAndPropagateRear, Event, Counter} -> 
            utils:log("Event readAndPropagateRear"),
            message:readAndPropagateRearHandler(Event, Counter, Data);
        defaultBehaviour ->
            utils:log("Event defaultBehaviour"),
            flow:callTowTruck(Data);
        {crossing, _WaitingCar} ->
            utils:log("Event crossing"),
            flow:next(crossing, Data, From);
        {newCar, front, NewCar} -> 
            utils:log("Event newCar front"),
            flow:keep(updateAdj(Data, Data#carState.adj#adj.frontCars ++ [NewCar]), From);
        {newCar, rear, NewCar} -> 
            utils:log("Event newCar rear"),
            flow:keep(updateAdj(Data, [NewCar | Data#carState.adj#adj.frontCars]), From);
	    leader ->
            utils:log("Event leader"),
            flow:next(leader, Data, From)
    end.


leader({call, From}, Event, Data) ->
    utils:log("STATE Queue"),
    case Event of
        engineCrash -> 
            utils:log("Event engineCrash"),
            flow:launchEvent(towTruck, [Data#carState.name, 2000]),
            flow:next(dead, Data, From);
        systemCrash -> 
            utils:log("Event systemCrash"),
            flow:next(dead, Data, From);
        check ->
            utils:log("Event check"),
            flow:keep(Data, From, ok);
        {propagateFront, Event, Counter} -> 
            utils:log("Event propagateFront"),
            message:propagateFrontHandler(Event, Counter, Data);
        {propagateRear, Event, Counter} -> 
            utils:log("Event propagateRear"),
            message:propagateRearHandler(Event, Counter, Data);
        {readAndPropagateFront, Event, Counter} -> 
            utils:log("Event readAndPropagateFront"),
            message:readAndPropagateFrontHandler(Event, Counter, Data);
        {readAndPropagateRear, Event, Counter} -> 
            utils:log("Event readAndPropagateRear"),
            message:readAndPropagateRearHandler(Event, Counter, Data);
        {crossing, _WaitingCar} ->
            utils:log("Event crossing"),
            flow:next(crossing, Data, From);
        {newCar, front, NewCar} -> 
            utils:log("Event newCar front"),
            flow:keep(updateAdj(Data, Data#carState.adj#adj.frontCars ++ [NewCar]), From);
        {newCar, rear, NewCar} -> 
            utils:log("Event newCar rear"),
            flow:keep(updateAdj(Data, [NewCar | Data#carState.adj#adj.frontCars]), From);
        defaultBehaviour ->
            utils:log("Event defaultBehaviour"),
            flow:callTowTruck(Data),
            WaitingCar = utils:lastElement(Data#carState.adj#adj.frontCars),
            message:readAndPropagateRearHandler({crossing, WaitingCar}, Data#carState.bridgeCapacity, Data),
            flow:keep(Data, From)
        end.


crossing({call, From}, Event, Data) ->
    utils:log("STATE Crossing"),
    case Event of
    engineCrash -> 
        utils:log("Event engineCrash"),
        flow:launchEvent(towTruck, [Data#carState.name, 2000]),
        flow:next(dead, Data, From);
    systemCrash -> 
        utils:log("Event systemCrash"),
        flow:next(dead, Data, From);
    check ->
        utils:log("Event check"),
        flow:keep(Data, From, ok);
    {propagateFront, Event, Counter} -> 
        utils:log("Event propagateFront"),
        message:propagateFrontHandler(Event, Counter, Data);
    {propagateRear, Event, Counter} -> 
        utils:log("Event propagateRear"),
        message:propagateRearHandler(Event, Counter, Data);
    {readAndPropagateFront, Event, Counter} -> 
        utils:log("Event readAndPropagateFront"),
        message:readAndPropagateFrontHandler(Event, Counter, Data);
    {readAndPropagateRear, Event, Counter} -> 
        utils:log("Event readAndPropagateRear"),
        message:readAndPropagateRearHandler(Event, Counter, Data);
    crossed ->
        utils:log("Event crossed"),
        flow:next(crossed, Data, From);
    defaultBehaviour ->
        utils:log("Event defaultBehaviour"),
        flow:callTowTruck(Data),
        flow:launchEvent(crossingTimer, [Data#carState.name, Data#carState.bridgeCrossingTime]),
        flow:keep(Data, From)
    end.


crossed({call, _From}, Event, Data) -> 
    utils:log("STATE Crossed"),
    case Event of
        defaultBehaviour ->
            utils:log("Event defaultBehaviour"),
            % notifica agli altri
            stop(Data#carState.name)
    end.


dead({call, _From}, Event, Data) -> 
    utils:log("STATE Dead"),
    case Event of
        defaultBehaviour ->
            utils:log("Event defaultBehaviour"),
            % notifica agli altri
            stop(Data#carState.name)
    end.