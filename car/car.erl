-module(car).
-behaviour(gen_statem).

-include("car.hrl"). 
-include("car_std.hrl").
-include("car_api.hrl").
-include("car_utils.hrl").


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% TODO il web service ha una lista con l'ordine di sincronizzazione
%% in fase di sincronizzazione tu chiami per la lista di sinc 
%% una volta sincronizzati fai l'update della lista delle adiacenze

init([Name, Side, Power, BridgeCapacity, BridgeCrossingTime, Timeout]) ->
    log("STATE Init - Broken car, Timeout:~p", [Timeout]),
    launchEvent(killer, [Name, Timeout]),
    launchEvent(launcher, [Name, defaultBehaviour]),
    log("STATE TRANSITION: Init -> Create"),
    {ok, create, #carState{
                            name = Name, 
                            side = Side, 
                            power = Power, 
                            adj = #adj{frontCars = getSyncAdj(Name, Side, Power), rearCars = []}, 
                            arrivalTime = getTimeStamp(), 
                            bridgeCapacity = BridgeCapacity, 
                            bridgeCrossingTime = BridgeCrossingTime
                        }};
init([Name, Side, Power, BridgeCapacity, BridgeCrossingTime]) ->
    log("STATE Init"),
    launchEvent(launcher, [Name, defaultBehaviour]),
    log("STATE TRANSITION: Init -> Create"),
    {ok, create, #carState{
                            name = Name, 
                            side = Side, 
                            power = Power, 
                            adj = #adj{frontCars = getSyncAdj(Name, Side, Power), rearCars = []}, 
                            arrivalTime = getTimeStamp(), 
                            bridgeCapacity = BridgeCapacity, 
                            bridgeCrossingTime = BridgeCrossingTime
                        }}.
        

create({call, From}, Event, Data) ->
    log("STATE Create"),
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
            log("STATE Create - Event sync"),
            no_sync;
        defaultBehaviour ->
            log("STATE Create - Event defaultBehaviour"),
            %callTowTruck(Data),
            if Data#carState.adj#adj.frontCars =/= [] ->
                log("Syncronize with front car"),
                %Pivot = lastElement(Data#carState.adj#adj.frontCars),
                %next(queue, updateDelta(Data, berkeley(Pivot)), From);
                keep(Data, From);
            true -> 
                log("The only car - no sync needed"),
                keep(Data, From)
                %launchEvent(launcher, [defaultBehaviour]),
                %next(leader, Data, From)
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
            log("STATE Create - Event defaultBehaviour"),
            callTowTruck(Data);
        {crossing, _WaitingCar} ->
            next(crossing, Data, From);
        {newCar, front, NewCar} -> 
            keep(updateAdj(Data, Data#carState.adj#adj.frontCars ++ [NewCar]), From);
        {newCar, rear, NewCar} -> 
            keep(updateAdj(Data, [NewCar | Data#carState.adj#adj.frontCars]), From);
	    leader ->
            log("STATE Queue - Event leader"),
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
            log("STATE Leader - Event defaultBehaviour"),
            callTowTruck(Data),
            WaitingCar = lastElement(Data#carState.adj#adj.frontCars),
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
        log("STATE Leader - Event defaultBehaviour"),
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
        Target = (lastElement(Data#carState.adj#adj.frontCars, Hop))#carState.name,
        sendEvent(Target, {propagateFront, Event, Counter - Hop});
    Counter == 1 -> 
        launchEvent(launcher, [Event])
    end.


propagateRearHandler(Event, Counter, Data) ->
    if Counter > 1 ->
        Hop = erlang:min(Counter, Data#carState.power),
        Target = (firstElement(Data#carState.adj#adj.rearCars, Hop))#carState.name,
        sendEvent(Target, {propagateRear, Event, Counter - Hop});
    Counter == 1 -> 
        launchEvent(launcher, [Event])
    end.


readAndPropagateFrontHandler(Event, Counter, Data) -> 
    if Counter > 1 ->
        launchEvent(launcher, [Event]),
        Hop = erlang:min(Counter, Data#carState.power),
        sendNearFrontCars(Event, Data, Hop - 1),
        Target = (lastElement(Data#carState.adj#adj.frontCars, Hop))#carState.name,
        sendEvent(Target, {readAndPropagateFront, Event, Counter - Hop});
    Counter == 1 ->
        launchEvent(launcher, [Event])
    end.


sendNearFrontCars(Event, Data, Hop) ->
    if Hop > 0 ->
        Target = (lastElement(Data#carState.adj#adj.frontCars, Hop))#carState.name,
        sendEvent(Target, Event),
        sendNearFrontCars(Event, Data, Hop - 1)
    end.


readAndPropagateRearHandler(Event, Counter, Data) -> 
    if Counter > 1 ->
        launchEvent(launcher, [Event]),
        Hop = erlang:min(Counter, Data#carState.power),
        sendNearRearCars(Event, Data, Hop - 1),
        Target = (firstElement(Data#carState.adj#adj.rearCars, Hop))#carState.name,
        sendEvent(Target, {readAndPropagateRear, Event, Counter - Hop});
    Counter == 1 ->
        launchEvent(launcher, [Event])
    end.
    
    
sendNearRearCars(Event, Data, Hop) ->
    if Hop > 0 ->
        Target = (firstElement(Data#carState.adj#adj.rearCars, Hop))#carState.name,
        sendEvent(Target, Event),
        sendNearRearCars(Event, Data, Hop - 1)
    end.


%%%===================================================================
%%% spawn support processes
%%%===================================================================


%% Simulate a car crash after a given timeout
killer(Name, Timeout) ->
    timer:apply_after(Timeout, gen_statem, call, [{global, Name}, crash]).

%% Spawn a process that launches an event
launchEvent(Handler, Args) -> 
    log("launchEvent: ~p~p~n", [Handler, Args]),
    spawn(?MODULE, Handler, Args).


callTowTruck(Data) ->
    Responses = sendToAllAdj(Data#carState.adj#adj.frontCars ++ Data#carState.adj#adj.rearCars, check),
    callTowTruckWrap(Responses).


callTowTruckWrap([{Car, Response} | Rest]) ->
    if Response =/= ok ->
        launchEvent(towTruck, [Car#carState.name, 3000])
    end,
    callTowTruckWrap(Rest).

%% Launch a given event until success (polling)
launcher(Name, Event) ->
    %gen_statem:call({global, Name}, Event).
    %io:format("aiuttoo"),
    try gen_statem:call({global, Name}, Event) of 
        _ -> { } 
    catch 
        exit:_ -> {launcher(Name, Event)}; 
        error:_ -> {launcher(Name, Event)};
        throw:_ -> {launcher(Name, Event)} 
    end. 


next(NextState, Data, From) ->
    log("STATE TRANSITION -> ~p", [NextState]),
    if NextState =/= dead ->
        launchEvent(launcher, [defaultBehaviour])
    end,
    {next_state, NextState, Data, [{reply, From, io:format(NextState)}]}.


next(NextState, Data, From, Reply) ->
    log("STATE TRANSITION -> ~p", [NextState]),
    if NextState =/= dead ->
        launchEvent(launcher, [defaultBehaviour])
    end,
    {next_state, NextState, Data, [{reply, From, Reply}]}.
        

keep(Data, From) ->
    log("KEEP STATE"),
    {keep_state, Data, [{reply, From, "keep_state"}]}.


keep(Data, From, Reply) ->
    log("KEEP STATE"),
    {keep_state, Data, [{reply, From, Reply}]}.