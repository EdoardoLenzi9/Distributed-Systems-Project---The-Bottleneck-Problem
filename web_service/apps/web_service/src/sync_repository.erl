-module(sync_repository).
-compile(export_all).
-include("entity.hrl").


%%%===================================================================
%%% public functions
%%%===================================================================

add(Entity) ->
    db_manager:add(Entity).


get_all() ->
    order(db_manager:get_all(syncEntity)).


%%%===================================================================
%%% private functions
%%%===================================================================

order(List) ->
    F = fun(X, Y) -> 
        if X#syncEntity.side == 1, Y#syncEntity.side == 1 -> 
            X#syncEntity.timeStamp < Y#syncEntity.timeStamp;
        X#syncEntity.side == 1, Y#syncEntity.side == -1 ->
            false;
        X#syncEntity.side == -1, Y#syncEntity.side == 1 ->
            true; 
        X#syncEntity.side == -1, Y#syncEntity.side == -1 -> 
            X#syncEntity.timeStamp > Y#syncEntity.timeStamp
            end
        end,
    lists:sort(F, List).