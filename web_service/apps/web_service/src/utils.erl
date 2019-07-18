-module(utils).
-compile(export_all).
-include("entity.hrl").
-define(LOG, true).


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


last_elements(List, Hop) ->
    lists:nthtail(length(List) - erlang:min(length(List), Hop), List).


first_elements([ ], _) ->
        [ ];
    first_elements([First | Rest], Hop) ->
        if Hop > 0 -> 
            [First | first_elements(Rest, Hop - 1)];
        true -> 
            [ ]
        end.


% [1,2,3] -> 3
last_element([ ]) ->
    undefined;
last_element(List) ->
    [Pivot] = lists:nthtail(length(List)-1, List),
    Pivot.


% [1,2,3] -> 1    
first_element([ ]) ->
    undefined;
first_element([First | _ ]) ->
    First.


get_timestamp() ->
    {Mega, Seconds, Ms} = os:timestamp(),
    (Mega*1000000 + Seconds)*1000 + erlang:round(Ms/1000).     


concat([]) ->
    [];
concat([First | Rest]) ->
    string:concat(First, concat(Rest)).


% Load environment.json
load_environment() ->
    os:cmd("echo ciao > a.txt"),
    {ok, Content} = file:read_file("environment.json"),
    {[{<<"host">>,_Host},
    {<<"port">>,_Port},
    {<<"process_visibility">>,ProcessVisibility},
    {<<"max_speed">>,MaxSpeed},
    {<<"bridge_capacity">>,BridgeCapacity},
    {<<"bridge_length">>,BridgeLength},
    {<<"tow_truck_time">>,TowTruckTime},
    {<<"max_RTT">>,MaxRTT}]} = jiffy:decode(Content),
    #settingsEntity{
        process_visibility = list_to_atom(binary_to_list(ProcessVisibility)),
        max_speed = MaxSpeed, 
        bridge_capacity = BridgeCapacity, 
        bridge_length = BridgeLength, 
        tow_truck_time = TowTruckTime,
        max_RTT = MaxRTT
    }.
