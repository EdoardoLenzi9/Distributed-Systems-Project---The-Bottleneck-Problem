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
    {<<"samplingFrequency">>,_SamplingFrequency},
    {<<"towTruckTime">>,TowTruckTime},
    {<<"maxRTT">>,MaxRTT}]} = jiffy:decode(Content),
    #env{
        host = binary_to_list(Host), 
        maxSpeed = MaxSpeed, 
        turn = Turn, 
        bridgeCapacity = BridgeCapacity, 
        bridgeLength = BridgeLength, 
        tow_truckTime = TowTruckTime,
        maxRTT = MaxRTT
    }.

%%%===================================================================
%%% list management
%%         Front        Rear
%%      [A, B, C] CAR [D, E, F]
%%%===================================================================

% [1,2,3] -> 3
last_element([ ]) ->
        -1;
last_element(List) ->
    [Pivot] = lists:nthtail(length(List)-1, List),
    Pivot.

last_element([ ], _) ->
        -1;
last_element(List, Hop) ->
    if Hop < length(List) ->
        [Pivot] = lists:nthtail(length(List) - Hop, List),
        Pivot;
    true ->
        -1
    end.


% [1,2,3] -> 1    
first_element([ ]) ->
    -1;
first_element([First | _ ]) ->
    First.

first_element([ ], _) ->
    -1;
first_element([First | _Rest], 1) ->
    First;
first_element([_First | Rest], Hop) ->
    if Hop < length(Rest) + 1 ->
        first_element(Rest, Hop - 1);
    true ->
        -1
    end.


%%%===================================================================
%%% time management
%%%===================================================================

get_timestamp() ->
    {Mega, Seconds, Ms} = os:timestamp(),
    (Mega*1000000 + Seconds)*1000 + erlang:round(Ms/1000).                                                                                                                                              