-module(adj_repository).
-compile(export_all).
-include("entity.hrl").


%%%===================================================================
%%% public functions
%%%===================================================================

add(Entity) ->
    db_manager:add(Entity).


get_all() ->
    order(db_manager:get_all(adjEntity)).


%%%===================================================================
%%% private functions
%%%===================================================================

order(List) ->
    F = fun(X, Y) -> 
        if X#adjEntity.side == right, Y#adjEntity.side == right -> 
            X#adjEntity.arrival_time + X#adjEntity.delta < Y#adjEntity.arrival_time + Y#adjEntity.delta;
        X#adjEntity.side == right, Y#adjEntity.side == left ->
            false;
        X#adjEntity.side == left, Y#adjEntity.side == right ->
            true; 
        X#adjEntity.side == left, Y#adjEntity.side == left -> 
            X#adjEntity.arrival_time + X#adjEntity.delta > Y#adjEntity.arrival_time + Y#adjEntity.delta
            end
        end,
    lists:sort(F, List).
