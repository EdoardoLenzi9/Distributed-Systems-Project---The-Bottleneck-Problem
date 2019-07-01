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
%propagate_front_handler(Event, Counter, Data) ->
%    if Counter > 1 ->
%        Hop = erlang:min(Counter, Data#carState.power),
%        Target = (utils:last_element(Data#carState.adj#adj.frontCars, Hop))#carState.name,
%        send_event(Target, {propagateFront, Event, Counter - Hop});
%    Counter == 1 -> 
%        flow:launch_event(launcher, [Data#carState.name, Event])
%    end.
%
%
%propagate_rear_handler(Event, Counter, Data) ->
%    if Counter > 1 ->
%        Hop = erlang:min(Counter, Data#carState.power),
%        Target = (utils:first_element(Data#carState.adj#adj.rearCars, Hop))#carState.name,
%        send_event(Target, {propagateRear, Event, Counter - Hop});
%    Counter == 1 -> 
%        flow:launch_event(launcher, [Data#carState.name, Event])
%    end.
%
%
%read_and_propagate_front_handler(Event, Counter, Data) -> 
%    if Counter > 1 ->
%        flow:launch_event(launcher, [Data#carState.name, Event]),
%        Hop = erlang:min(Counter, Data#carState.power),
%        send_near_front_cars(Event, Data, Hop - 1),
%        Target = utils:last_element(Data#carState.adj#adj.frontCars, Hop),
%        if Target =/= [] ->
%            TargetName = Target#carState.name,
%            send_event(TargetName, {readAndPropagateFront, Event, Counter - Hop});
%        true ->
%            []
%        end;
%    Counter == 1 ->
%        flow:launch_event(launcher, [Data#carState.name, Event])
%    end.
%
%
%send_near_front_cars(Event, Data, Hop) ->
%    if Hop > 0 ->
%        Target = utils:last_element(Data#carState.adj#adj.frontCars, Hop),
%        if Target =/= [] ->
%            TargetName = Target#carState.name,
%            send_event(TargetName, Event),
%            send_near_front_cars(Event, Data, Hop - 1);
%        true ->
%            []
%        end;
%    true ->
%        ok
%    end.
%
%
%read_and_propagate_rear_handler(Event, Counter, Data) -> 
%    if Counter > 1 ->
%        flow:launch_event(launcher, [Data#carState.name, Event]),
%        Hop = erlang:min(Counter, Data#carState.power),
%        send_near_rear_cars(Event, Data, Hop - 1),
%        Target = utils:first_element(Data#carState.adj#adj.rearCars, Hop),
%        if Target =/= -1 ->
%            TargetName = Target#carState.name,
%            send_event(TargetName, {readAndPropagateRear, Event, Counter - Hop});
%        true ->
%            []
%        end;
%    Counter == 1 ->
%        flow:launch_event(launcher, [Data#carState.name, Event])
%    end.
%    
%    
%send_near_rear_cars(Event, Data, Hop) ->
%    if Hop > 0 ->
%        Target = utils:first_element(Data#carState.adj#adj.rearCars, Hop),
%        if Target =/= [] ->
%            TargetName = Target#carState.name,
%            send_event(TargetName, Event),
%            send_near_rear_cars(Event, Data, Hop - 1);
%        true ->
%            []
%        end;
%    true -> 
%        ok
%    end.
%
%
%send_to_all_adjWrap([], _Event) -> 
%    [];
%send_to_all_adjWrap([Car], Event) -> 
%    send_event(Car#carState.name, Event);
%send_to_all_adjWrap([FirstCar | Rest], Event) -> 
%    Response = send_event(FirstCar#carState.name, Event),
%    [send_to_all_adjWrap(Rest, Event) | Response].
%
%
%send_to_all_adj(List, Event) -> 
%    Responses = send_to_all_adjWrap(List, Event),
%    if length(Responses) > 1 -> 
%        [[First] | Rest] = Responses,
%        [First | Rest ];
%    true -> 
%        Responses
%    end.