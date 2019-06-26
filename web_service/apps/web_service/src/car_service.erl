-module(car_service).
-compile(export_all).
-include("entity.hrl").

add_sync(Name, Side, Power) ->
    Sync = db_manager:get_all(syncEntity),
    Adj = if Side == left -> 
        firstElements(Sync, Power);
    Side == right ->
        lastElements(Sync, Power)
    end,    
db_manager:add(#syncEntity{timeStamp = db_manager:getTimeStamp(), name = Name, side = Side, power = Power}).


lastElements(List, Hop) ->
    if length(List) > Hop ->
        lists:nthtail(length(List) - Hop, List);
    true -> 
        List
    end.


firstElements(List, Hop) -> 
    Result = firstElementsWrapper(List, Hop),
    if length(Result) > 0 ->
        [[], Rest] = Result,
        Rest;
    true ->
        []
    end.

firstElementsWrapper([ ], _) ->
    [ ];
firstElementsWrapper([First | Rest], Hop) ->
    if Hop > 0 -> 
        [firstElementsWrapper(Rest, Hop - 1) | First];
    true -> 
        [ ]
    end.