-module(sync_repository).
-compile(export_all).
-include("entity.hrl").


%%%===================================================================
%%% public functions
%%%===================================================================

add(Entity) ->
    F = fun() -> 
        FrontCars = db_manager:select(sync_entity, #sync_entity{side = Entity#sync_entity.side, chained = false, _='_'}, [], ['$_']),
        Return = if length(FrontCars) == 1 ->
            [FrontCar] = FrontCars,
            db_manager:add(FrontCar#sync_entity{chained = true}),
            FrontCar;
        true ->
            OppositeCars = db_manager:select(sync_entity, #sync_entity{side = (Entity#sync_entity.side * -1), _='_'}, [], ['$_']),
            if length(OppositeCars) > 0 ->
                [OppositeCar | _Rest] = order(OppositeCars),
                OppositeCar;
            true ->
                []
            end
        end,
        Index = db_manager:counter(sync_entity) + 1,
        db_manager:add(Entity#sync_entity{ time_stamp = Index * Entity#sync_entity.side, chained = false }),
        Return
    end,
    {atomic, Res} = mnesia:transaction(F),
    Res.
    

delete(Entity) ->
    Result = repository_helper:select(sync_entity, #sync_entity{name = Entity#sync_entity.name, _='_'}, [], ['$_']),
    if length(Result) == 1 ->
        [Item] = Result,
        utils:log("Delete sync record ~p", [Item]),
        repository_helper:delete(Item);
    true ->
        ok 
    end.
        


select(Name) ->
    F = fun() -> 
        SelectResult = db_manager:select(sync_entity, #sync_entity{name = Name, _='_'}, [], ['$_']),
        if length(SelectResult) == 1 ->
            SelectResult;
        true ->
            []
        end
    end,
    {atomic, Res} = mnesia:transaction(F),
    Res.


get_all() ->
    order(repository_helper:get_all(sync_entity)).


%%%===================================================================
%%% private functions
%%%===================================================================

order(List) ->
    F = fun(X, Y) -> 
        if X#sync_entity.side == 1, Y#sync_entity.side == 1 -> 
            X#sync_entity.time_stamp < Y#sync_entity.time_stamp;
        X#sync_entity.side == 1, Y#sync_entity.side == -1 ->
            false;
        X#sync_entity.side == -1, Y#sync_entity.side == 1 ->
            true; 
        X#sync_entity.side == -1, Y#sync_entity.side == -1 -> 
            X#sync_entity.time_stamp > Y#sync_entity.time_stamp
            end
        end,
    lists:sort(F, List).