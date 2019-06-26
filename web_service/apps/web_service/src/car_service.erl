-module(car_service).
-compile(export_all).
-include("entity.hrl").

add_sync(Name, Side, Power) ->
    Sync = db_manager:get_all(syncEntity),
    io:format("~n~p", [Sync]),
    io:format("~n~p", [Power]),
    io:format("~n~p", [Side]),
    io:format("~n~p", [Name]),
    Adj = if Side == "left" -> 
        firstElements(Sync, Power);
    Side == "right" ->
        lastElements(Sync, Power)
    end,    
    io:format("~n~p", [Adj]),

    %db_manager:clear(syncEntity),
%    db_manager:addRange(Sync),
    db_manager:add(#syncEntity{timeStamp = db_manager:getTimeStamp(), name = Name, side = Side, power = Power}),

    %io:format("~n~n~n ~p ~n~n~n", [sync_marshalling(Adj)]),
    sync_marshalling(Adj).


lastElements(List, Hop) ->
    if length(List) > Hop ->
        lists:nthtail(length(List) - Hop, List);
    true -> 
        List
    end.


firstElements([ ], _) ->
        [ ];
    firstElements([First | Rest], Hop) ->
        if Hop > 0 -> 
            [First | firstElements(Rest, Hop - 1)];
        true -> 
            [ ]
        end.