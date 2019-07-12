-module(adj_repository).
-compile(export_all).
-include("entity.hrl").


%%%===================================================================
%%% public functions
%%%===================================================================

add(Entity) ->
    db_manager:add(Entity).


delete(Entity) ->
    db_manager:delete(Entity).

get_all() ->
    order(db_manager:get_all(adjEntity)).


%%%===================================================================
%%% private functions
%%%===================================================================

order(List) ->
    F = fun(X, Y) -> 
        if X#adjEntity.side == 1, Y#adjEntity.side == 1 -> 
            X#adjEntity.arrival_time + X#adjEntity.delta < Y#adjEntity.arrival_time + Y#adjEntity.delta;
        X#adjEntity.side == 1, Y#adjEntity.side == -1 ->
            false;
        X#adjEntity.side == -1, Y#adjEntity.side == 1 ->
            true; 
        X#adjEntity.side == -1, Y#adjEntity.side == -1 -> 
            X#adjEntity.arrival_time + X#adjEntity.delta > Y#adjEntity.arrival_time + Y#adjEntity.delta
            end
        end,
    lists:sort(F, List).
