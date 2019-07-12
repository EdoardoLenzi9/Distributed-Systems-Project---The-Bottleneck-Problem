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
    order(db_manager:get_all(adj_entity)).


%%%===================================================================
%%% private functions
%%%===================================================================

order(List) ->
    F = fun(X, Y) -> 
        if X#adj_entity.side == 1, Y#adj_entity.side == 1 -> 
            X#adj_entity.arrival_time + X#adj_entity.delta < Y#adj_entity.arrival_time + Y#adj_entity.delta;
        X#adj_entity.side == 1, Y#adj_entity.side == -1 ->
            false;
        X#adj_entity.side == -1, Y#adj_entity.side == 1 ->
            true; 
        X#adj_entity.side == -1, Y#adj_entity.side == -1 -> 
            X#adj_entity.arrival_time + X#adj_entity.delta > Y#adj_entity.arrival_time + Y#adj_entity.delta
            end
        end,
    lists:sort(F, List).
