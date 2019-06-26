-module(car_service).
-compile(export_all).
-include("entity.hrl").

add_sync(Name, Side, Power) ->
    Sync = order(db_manager:get_all(syncEntity)),
    Adj = if Side == "left" -> 
        firstElements(Sync, Power);
    Side == "right" ->
        lists:reverse(lastElements(Sync, Power))
    end,    
    db_manager:add(#syncEntity{timeStamp = db_manager:getTimeStamp(), name = Name, side = Side, power = Power}),
    sync_marshalling(Adj).


lastElements(List, Hop) ->
    lists:nthtail(length(List) - erlang:min(length(List), Hop), List).


firstElements([ ], _) ->
        [ ];
    firstElements([First | Rest], Hop) ->
        if Hop > 0 -> 
            [First | firstElements(Rest, Hop - 1)];
        true -> 
            [ ]
        end.


order(List) ->
    F = fun(X, Y) -> 
        if X#syncEntity.side == "right", Y#syncEntity.side == "right" -> 
            X#syncEntity.timeStamp < Y#syncEntity.timeStamp;
        X#syncEntity.side == "right", Y#syncEntity.side == "left" ->
            false;
        X#syncEntity.side == "left", Y#syncEntity.side == "right" ->
            true; 
        X#syncEntity.side == "left", Y#syncEntity.side == "left" -> 
            X#syncEntity.timeStamp > Y#syncEntity.timeStamp
            end
        end,
    lists:sort(F, List).