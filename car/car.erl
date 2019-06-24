-module(car).
-behaviour(gen_statem).

-include("car.hrl"). 
-include("car_std.hrl").
-include("car_api.hrl").
-include("car_utils.hrl").

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================


init([BridgeLength, Timeout]) ->
    log("STATE Init - Broken car, Timeout:~p", [Timeout]),
    Adj = #adj{frontCars = [], rearCars = []},
    launchEvent(killer, [Timeout]),
    launchEvent(launcher, [init]),
    {ok, create, #carState{adj = Adj, arrivalTime=1, bridgeLength = BridgeLength, timeout=0}};
init([BridgeLength]) ->
    log("STATE Init"),
    Adj = #adj{frontCars = [], rearCars = []},
    launchEvent(launcher, [defaultBehaviour]),
    log("STATE TRANSITION: Init -> Create"),
    {ok, create, #carState{adj = Adj, arrivalTime=1, bridgeLength = BridgeLength, timeout=0}}.
        

create({call, From}, Event, Data) ->
    log("STATE Create"),
    case Event of
        crash -> 
            next(dead,  updateTimeout(Data, 5000), From);
        sync ->
            log("STATE Create - Event sync"),
            no_sync;
        defaultBehaviour ->
            log("STATE Create - Event init"),
            if Data#carState.adj#adj.frontCars =/= [] ->
                log("Syncronize with front car"),
                Pivot = lastElement(Data#carState.adj#adj.frontCars),
                next(queue, updateDelta(Data, berkeley(Pivot)), From);
            true -> 
                launchEvent(launcher, [defaultBehaviour]),
                next(leader, Data, From)
            end
    end.


queue({call, From}, Event, Data) ->
    case Event of
        crash ->
            next(dead, updateTimeout(Data, 5000), From);
        move ->
            log("STATE Queue - Event move"),
            next(crossing, updateTimeout(Data, 10000), From);
	    leader ->
            log("STATE Queue - Event leader"),
            next(leader, Data, From)
    end.


leader({call, From}, Event, Data) ->
    case Event of
        crash ->
            next(dead, updateTimeout(Data, 5000), From);
        {propagateFront, Event, Counter} -> 
            % TODO potenza di propagazione
            if Counter > 1 ->
                Target = (lastElement(Data#carState.adj#adj.frontCars))#carState.name,
                sendEvent(Target, {propagateFront, Event, Counter -1});
            Counter == 1 -> 
                launchEvent(launcher, [Event])
            end;
        {propagateRear, Event, Counter} -> 
            if Counter > 1 ->
                Target = (firstElement(Data#carState.adj#adj.rearCars))#carState.name,
                sendEvent(Target, {propagateRear, Event, Counter -1});
            Counter == 1 -> 
                launchEvent(launcher, [Event])
            end;
        {readAndPropagateFront, Event, Counter} -> 
            if Counter > 1 ->
                launchEvent(launcher, [Event]),
                Target = (lastElement(Data#carState.adj#adj.frontCars))#carState.name,
                sendEvent(Target, {readAndPropagateFront, Event, Counter -1})
            end;
        {readAndPropagateRear, Event, Counter} -> 
            if Counter > 1 ->
                launchEvent(launcher, [Event]),
                Target = (firstElement(Data#carState.adj#adj.rearCars))#carState.name,
                sendEvent(Target, {readAndPropagateRear, Event, Counter -1})
            end;
        {newCar, front, NewCar} -> 
            keep(updateAdj(Data, Data#carState.adj#adj.frontCars ++ [NewCar]), From);
        {newCar, rear, NewCar} -> 
            keep(updateAdj(Data, [NewCar | Data#carState.adj#adj.frontCars]), From);
        defaultBehaviour ->
            % controllo che non ci sia nessuno davanti
            if Data#carState.adj#adj.frontCars == [] ->
                notifyCrossing(Data#carState.bridgeLength - 1, Data#carState.adj#adj.frontCars),
                next(crossing, updateTimeout(Data, 10000), From);
            true ->
                keep(Data, From)
            end
    end.


crossing({call, From}, Event, Data) ->
    case Event of
    crash ->
        next(dead, updateTimeout(Data, 5000), From);
	timeout ->
	    log("car crossed the bridge~n"),
	    stop()
    end.
    

dead({call, _From}, Event, _Data) ->
    case Event of
   	timeout ->
	   log("the car has been removed~n"),
	   stop()
    end.
 
