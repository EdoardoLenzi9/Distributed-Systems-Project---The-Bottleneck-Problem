-module(sync_repository).
-compile(export_all).
-include("entity.hrl").


%%%===================================================================
%%% public functions
%%%===================================================================

add(Entity) ->
    db_manager:add(Entity).


delete(Entity) ->
    [Result] = db_manager:select(sync_entity, #sync_entity{name = Entity#sync_entity.name, _='_'}, [], ['$_']),
    utils:log("Delete sync record ~p", [Result]),
    db_manager:delete(Result).


get_all() ->
    order(db_manager:get_all(sync_entity)).


%%%===================================================================
%%% private functions
%%%===================================================================

order(List) ->
    F = fun(X, Y) -> 
        if X#sync_entity.side == 1, Y#sync_entity.side == 1 -> 
            X#sync_entity.timeStamp < Y#sync_entity.timeStamp;
        X#sync_entity.side == 1, Y#sync_entity.side == -1 ->
            false;
        X#sync_entity.side == -1, Y#sync_entity.side == 1 ->
            true; 
        X#sync_entity.side == -1, Y#sync_entity.side == -1 -> 
            X#sync_entity.timeStamp > Y#sync_entity.timeStamp
            end
        end,
    lists:sort(F, List).