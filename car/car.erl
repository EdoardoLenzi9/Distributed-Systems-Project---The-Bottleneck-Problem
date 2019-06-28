-module(car).
-behaviour(gen_statem).

-include("car.hrl"). 
-include("car_std.hrl").
-include("car_api.hrl").


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% TODO il web service ha una lista con l'ordine di sincronizzazione
%% in fase di sincronizzazione tu chiami per la lista di sinc 
%% una volta sincronizzati fai l'update della lista delle adiacenze

init([Name, Side, Power, Turn, BridgeCapacity, BridgeCrossingTime, Timeout]) ->
    utils:log("STATE Init - Broken car, Timeout:~p", [Timeout]),
    launchEvent(killer, [Name, Timeout]),
    launchEvent(launcher, [Name, defaultBehaviour]),
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
    launchEvent(launcher, [Name, defaultBehaviour]),
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
            launchEvent(towTruck, [2000]),
            next(dead, Data, From);
        systemCrash -> 
            next(dead, Data, From);
        check ->
            keep(Data, From, ok);
        {propagateFront, Event, Counter} -> 
            propagateFrontHandler(Event, Counter, Data);
        {propagateRear, Event, Counter} -> 
            propagateRearHandler(Event, Counter, Data);
        sync ->
            utils:log("STATE Create - Event sync"),
            no_sync;
        defaultBehaviour ->
            utils:log("STATE Create - Event defaultBehaviour"),
            callTowTruck(Data),
            if Data#carState.adj#adj.frontCars =/= [] ->
                utils:log("Syncronize with front car"),
                Pivot = utils:lastElement(Data#carState.adj#adj.frontCars),
                next(queue, updateDelta(Data, utils:berkeley(Pivot)), From);
            true -> 
                utils:log("The only car - no sync needed"),
                next(leader, Data, From)
            end
    end.


queue({call, From}, Event, Data) ->
    case Event of
        crash ->
            next(dead, Data, From);
        {propagateFront, Event, Counter} -> 
            propagateFrontHandler(Event, Counter, Data);
        {propagateRear, Event, Counter} -> 
            propagateRearHandler(Event, Counter, Data);
        {readAndPropagateFront, Event, Counter} -> 
            readAndPropagateFrontHandler(Event, Counter, Data);
        {readAndPropagateRear, Event, Counter} -> 
            readAndPropagateRearHandler(Event, Counter, Data);
        defaultBehaviour ->
            utils:log("STATE Create - Event defaultBehaviour"),
            callTowTruck(Data);
        {crossing, _WaitingCar} ->
            next(crossing, Data, From);
        {newCar, front, NewCar} -> 
            keep(updateAdj(Data, Data#carState.adj#adj.frontCars ++ [NewCar]), From);
        {newCar, rear, NewCar} -> 
            keep(updateAdj(Data, [NewCar | Data#carState.adj#adj.frontCars]), From);
	    leader ->
            utils:log("STATE Queue - Event leader"),
            next(leader, Data, From)
    end.


leader({call, From}, Event, Data) ->
    case Event of
        crash ->
            next(dead, Data, From);
        {propagateFront, Event, Counter} -> 
            propagateFrontHandler(Event, Counter, Data),
            keep(Data, From);
        {propagateRear, Event, Counter} -> 
            propagateRearHandler(Event, Counter, Data),
            keep(Data, From);
        {readAndPropagateFront, Event, Counter} -> 
            readAndPropagateFrontHandler(Event, Counter, Data),
            keep(Data, From);
        {readAndPropagateRear, Event, Counter} -> 
            readAndPropagateRearHandler(Event, Counter, Data),
            keep(Data, From);
        {crossing, _WaitingCar} ->
            next(crossing, Data, From);
        {newCar, front, NewCar} -> 
            keep(updateAdj(Data, Data#carState.adj#adj.frontCars ++ [NewCar]), From);
        {newCar, rear, NewCar} -> 
            keep(updateAdj(Data, [NewCar | Data#carState.adj#adj.frontCars]), From);
        defaultBehaviour ->
            utils:log("STATE Leader - Event defaultBehaviour"),
            callTowTruck(Data),
            WaitingCar = utils:lastElement(Data#carState.adj#adj.frontCars),
            readAndPropagateRearHandler({crossing, WaitingCar}, Data#carState.bridgeCapacity, Data),
            keep(Data, From)
        end.


crossing({call, From}, Event, Data) ->
    case Event of
    crash ->
        next(dead, Data, From);
    {propagateFront, Event, Counter} -> 
        propagateFrontHandler(Event, Counter, Data);
    {propagateRear, Event, Counter} -> 
        propagateRearHandler(Event, Counter, Data);
    {readAndPropagateFront, Event, Counter} -> 
        readAndPropagateFrontHandler(Event, Counter, Data);
    {readAndPropagateRear, Event, Counter} -> 
        readAndPropagateRearHandler(Event, Counter, Data);
    defaultBehaviour ->
        utils:log("STATE Leader - Event defaultBehaviour"),
        callTowTruck(Data)
    end.


dead({call, _From}, Event, Data) -> 
    case Event of
        removed ->
            % notifica agli altri
            stop(Data#carState.name)
    end.


%%%===================================================================
%%% generic event handling
%%%===================================================================

% TODO se arriva una nuova macchina propaga un messaggio a TUTTI con il numero di salti 
    % se la potenza e maggiore del numero di salti allora chiamo il web service per fare l'update della lista delle adiacenze
% TODO se invio il check a distanza tot e un destinatario non mi puo rispondere direttamente?!?
% ordine non serve 
propagateFrontHandler(Event, Counter, Data) ->
    if Counter > 1 ->
        Hop = erlang:min(Counter, Data#carState.power),
        Target = (utils:lastElement(Data#carState.adj#adj.frontCars, Hop))#carState.name,
        sendEvent(Target, {propagateFront, Event, Counter - Hop});
    Counter == 1 -> 
        launchEvent(launcher, [Event])
    end.


propagateRearHandler(Event, Counter, Data) ->
    if Counter > 1 ->
        Hop = erlang:min(Counter, Data#carState.power),
        Target = (utils:firstElement(Data#carState.adj#adj.rearCars, Hop))#carState.name,
        sendEvent(Target, {propagateRear, Event, Counter - Hop});
    Counter == 1 -> 
        launchEvent(launcher, [Event])
    end.


readAndPropagateFrontHandler(Event, Counter, Data) -> 
    if Counter > 1 ->
        launchEvent(launcher, [Event]),
        Hop = erlang:min(Counter, Data#carState.power),
        sendNearFrontCars(Event, Data, Hop - 1),
        Target = utils:lastElement(Data#carState.adj#adj.frontCars, Hop),
        if Target =/= [] ->
            TargetName = Target#carState.name,
            sendEvent(TargetName, {readAndPropagateFront, Event, Counter - Hop});
        true ->
            []
        end;
    Counter == 1 ->
        launchEvent(launcher, [Event])
    end.


sendNearFrontCars(Event, Data, Hop) ->
    if Hop > 0 ->
        Target = utils:lastElement(Data#carState.adj#adj.frontCars, Hop),
        if Target =/= [] ->
            TargetName = Target#carState.name,
            sendEvent(TargetName, Event),
            sendNearFrontCars(Event, Data, Hop - 1);
        true ->
            []
        end
    end.


readAndPropagateRearHandler(Event, Counter, Data) -> 
    if Counter > 1 ->
        launchEvent(launcher, [Event]),
        Hop = erlang:min(Counter, Data#carState.power),
        sendNearRearCars(Event, Data, Hop - 1),
        Target = utils:firstElement(Data#carState.adj#adj.rearCars, Hop),
        if Target =/= [] ->
            TargetName = Target#carState.name,
            sendEvent(TargetName, {readAndPropagateRear, Event, Counter - Hop});
        true ->
            []
        end;
    Counter == 1 ->
        launchEvent(launcher, [Event])
    end.
    
    
sendNearRearCars(Event, Data, Hop) ->
    if Hop > 0 ->
        Target = utils:firstElement(Data#carState.adj#adj.rearCars, Hop),
        if Target =/= [] ->
            TargetName = Target#carState.name,
            sendEvent(TargetName, Event),
            sendNearRearCars(Event, Data, Hop - 1);
        true ->
            []
        end
    end.


%%%===================================================================
%%% spawn support processes
%%%===================================================================


%% Simulate a car crash after a given timeout
killer(Name, Timeout) ->
    timer:apply_after(Timeout, gen_statem, call, [{global, Name}, crash]).

%% Spawn a process that launches an event
launchEvent(Handler, Args) -> 
    utils:log("launchEvent: ~p~p~n", [Handler, Args]),
    spawn(?MODULE, Handler, Args).


callTowTruck(Data) ->
    Responses = sendToAllAdj(Data#carState.adj#adj.frontCars ++ Data#carState.adj#adj.rearCars, check),
    callTowTruckWrap(Responses).

callTowTruckWrap([]) ->
    [];
callTowTruckWrap([{Car, Response} | Rest]) ->
    if Response =/= ok ->
        launchEvent(towTruck, [Car#carState.name, 3000])
    end,
    callTowTruckWrap(Rest).

%% Launch a given event until success (polling)
launcher(Name, Event) ->
    timer:apply_after(500, gen_statem, call, [{global, Name}, Event]).
    %gen_statem:call({global, Name}, Event).
    %io:format("aiuttoo"),
    %try gen_statem:call({global, Name}, Event) of 
    %    _ -> { } 
    %catch 
    %    exit:_ -> {launcher(Name, Event)}; 
    %    error:_ -> {launcher(Name, Event)};
    %    throw:_ -> {launcher(Name, Event)} 
    %end. 


next(NextState, Data, From) ->
    utils:log("STATE TRANSITION -> ~p", [NextState]),
    if NextState =/= dead ->
        launchEvent(launcher, [Data#carState.name, defaultBehaviour])
    end,
    {next_state, NextState, Data, [{reply, From, atom_to_list(NextState)}]}.


next(NextState, Data, From, Reply) ->
    utils:log("STATE TRANSITION -> ~p", [NextState]),
    if NextState =/= dead ->
        launchEvent(launcher, [Data#carState.name, defaultBehaviour])
    end,
    {next_state, NextState, Data, [{reply, From, Reply}]}.
        

keep(Data, From) ->
    utils:log("KEEP STATE"),
    {keep_state, Data, [{reply, From, "keep_state"}]}.


keep(Data, From, Reply) ->
    utils:log("KEEP STATE"),
    {keep_state, Data, [{reply, From, Reply}]}.