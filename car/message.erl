%%%===================================================================
%%% generic event handling
%%%===================================================================

-module(message).
-compile(export_all).
-include("car.hrl"). 

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
        flow:launchEvent(launcher, [Event])
    end.


propagateRearHandler(Event, Counter, Data) ->
    if Counter > 1 ->
        Hop = erlang:min(Counter, Data#carState.power),
        Target = (utils:firstElement(Data#carState.adj#adj.rearCars, Hop))#carState.name,
        sendEvent(Target, {propagateRear, Event, Counter - Hop});
    Counter == 1 -> 
        flow:launchEvent(launcher, [Event])
    end.


readAndPropagateFrontHandler(Event, Counter, Data) -> 
    if Counter > 1 ->
        flow:launchEvent(launcher, [Event]),
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
        flow:launchEvent(launcher, [Event])
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
        flow:launchEvent(launcher, [Event]),
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
        flow:launchEvent(launcher, [Event])
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
