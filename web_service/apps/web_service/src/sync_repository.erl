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
        if X#syncEntity.side == right, Y#syncEntity.side == right -> 
            X#syncEntity.timeStamp < Y#syncEntity.timeStamp;
        X#syncEntity.side == right, Y#syncEntity.side == left ->
            false;
        X#syncEntity.side == left, Y#syncEntity.side == right ->
            true; 
        X#syncEntity.side == left, Y#syncEntity.side == left -> 
            X#syncEntity.timeStamp > Y#syncEntity.timeStamp
            end
        end,
    lists:sort(F, List).