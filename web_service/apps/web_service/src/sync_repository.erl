-module(sync_repository).
-compile(export_all).
-include("entity.hrl").


add(Name, Side, Power) ->
    db_manager:add(#syncEntity{timeStamp = utils:get_timestamp(), name = Name, side = Side, power = Power}).


get_all() ->
    order(db_manager:get_all(syncEntity)).


get_first(Counter) ->
    Sync = get_all(),
    utils:first_elements(Sync, Counter).


get_last(Counter) ->
    Sync = get_all(),
    utils:last_elements(Sync, Counter).


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