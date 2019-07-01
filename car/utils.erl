%%%===================================================================
%%% generic utility functions, environment
%%%===================================================================
-module(utils).
-compile(export_all).
-define(LOG, true).
-include("car.hrl").

%% Logger
-ifdef(LOG).
    log(String)->
        ParsedString = io:format(String),
        io:format("~n~p~n", [ParsedString]).
    log(String, Args) ->
        ParsedString = io:format(String, Args),
        io:format("~n~p~n", [ParsedString]).
-else.
    log(String)-> ok.
    log(String, Args) -> ok.
-endif.


binary_to_atom(Item) ->
    list_to_atom(binary_to_list(Item)).


load_environment() ->
    {ok, Content} = file:read_file("environment.json"),
    {[{<<"host">>,Host},
    {<<"maxSpeed">>,MaxSpeed},
    {<<"turn">>,Turn},
    {<<"bridgeCapacity">>,BridgeCapacity},
    {<<"bridgeLength">>,BridgeLength},
    {<<"samplingFrequency">>,SamplingFrequency},
    {<<"towTruckTime">>,TowTruckTime}]} = jiffy:decode(Content),
    #env{
        host = binary_to_list(Host), 
        maxSpeed = MaxSpeed, 
        turn = Turn, 
        bridgeCapacity = BridgeCapacity, 
        bridgeLength = BridgeLength, 
        towTruckTime = TowTruckTime
    }.

%%%===================================================================
%%% list management
%%         Front        Rear
%%      [A, B, C] CAR [D, E, F]
%%%===================================================================

% [1,2,3] -> 3
lastElement([ ]) ->
        -1;
lastElement(List) ->
    [Pivot] = lists:nthtail(length(List)-1, List),
    Pivot.

lastElement([ ], _) ->
        -1;
lastElement(List, Hop) ->
    if Hop < length(List) ->
        [Pivot] = lists:nthtail(length(List) - Hop, List),
        Pivot;
    true ->
        -1
    end.


% [1,2,3] -> 1    
firstElement([ ]) ->
    -1;
firstElement([First | _ ]) ->
    First.

firstElement([ ], _) ->
    -1;
firstElement([First | _Rest], 1) ->
    First;
firstElement([_First | Rest], Hop) ->
    if Hop < length(Rest) + 1 ->
        firstElement(Rest, Hop - 1);
    true ->
        -1
    end.


%%%===================================================================
%%% time management
%%%===================================================================

getTimeStamp() ->
    {Mega, Seconds, Ms} = os:timestamp(),
    (Mega*1000000 + Seconds)*1000 + erlang:round(Ms/1000).                                                                                                                                              


berkeley(State, FrontCars) ->
    [Pivot] = FrontCars,
    getPivotTime(State, Pivot),
    1.
    %{CurrentTime, PivotTime} = getPivotTime(State, Pivot),
    %CurrentTime2 = getTimeStamp(),
    %RTT = CurrentTime2 - CurrentTime,
    %CurrentTime2 - (PivotTime + RTT / 2).


getPivotTime(State, Pivot) -> 
    CurrentTime = getTimeStamp(), 
    register(car, self()),
    case car_supervisor_api:check(State, Pivot) of
        no_response -> 
            io:format("no respoooonse~n~n", []);
            %{CurrentTime, CurrentTime};
        Check ->
            io:format("monazzooo~n~n", [])
            %Check
    end.

