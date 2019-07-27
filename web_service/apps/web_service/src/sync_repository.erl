-module(sync_repository).
-compile(export_all).
-include("entity.hrl").


%%%===================================================================
%%% public functions
%%%===================================================================

add(Entity) ->
    F = fun() -> 
        FrontCars = db_manager:select(sync_entity, #sync_entity{side = Entity#sync_entity.side, rear_car = undefined, _='_'}, [], ['$_']),
        Return = if length(FrontCars) == 1 ->
            [FrontCar] = FrontCars,
            db_manager:add(FrontCar#sync_entity{rear_car = Entity#sync_entity.name}),
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
        if Return =/= [] ->
            db_manager:add(Entity#sync_entity{ time_stamp = Index * Entity#sync_entity.side, rear_car = undefined, front_car = Return#sync_entity.name });
        true ->
            db_manager:add(Entity#sync_entity{ time_stamp = Index * Entity#sync_entity.side, rear_car = undefined, front_car = undefined })
        end,
        Return
    end,
    {atomic, Res} = mnesia:transaction(F),
    Res.
    

delete(Entity) ->
    F = fun() -> 
        Result = repository_helper:select(sync_entity, #sync_entity{name = Entity#sync_entity.name, _='_'}, [], ['$_']),
        if length(Result) == 1 ->
            [Item] = Result,
            utils:log("Delete sync record ~p", [Item]),

            RearCar = if Item#sync_entity.rear_car =/= undefined ->
                utils:log("Item chained with a rear car ~p", [Item#sync_entity.rear_car]),
                RearCars = repository_helper:select(sync_entity, #sync_entity{name = Item#sync_entity.rear_car, _='_'}, [], ['$_']),
                if length(RearCars) == 1 ->
                    [RearCar2] = RearCars,
                    RearCar2;
                true ->
                    undefined
                end;
            true ->
                undefined
            end,

            FrontCar = if Item#sync_entity.front_car =/= undefined ->
                utils:log("Item chained with a front car ~p", [Item#sync_entity.front_car]),
                FrontCars = repository_helper:select(sync_entity, #sync_entity{name = Item#sync_entity.front_car, _='_'}, [], ['$_']),
                if length(FrontCars) == 1 ->
                    [FrontCar2] = FrontCars,
                    FrontCar2;
                true ->
                    undefined
                end;
            true ->
                undefined
            end,
        
            if FrontCar =/= undefined ->

                if RearCar =/= undefined ->
                    db_manager:add(FrontCar#sync_entity{rear_car = RearCar#sync_entity.name}),
                    db_manager:add(RearCar#sync_entity{front_car = FrontCar#sync_entity.name});
                true ->
                    db_manager:add(FrontCar#sync_entity{rear_car = RearCar})
                end;

            true ->

                if RearCar =/= undefined ->
                    db_manager:add(RearCar#sync_entity{front_car = FrontCar});
                true ->
                    utils:log("Both, front car and rear car are undefined")
                end
            end,

            db_manager:delete(Item);
        true ->
            ok 
        end
    end,
    {atomic, Res} = mnesia:transaction(F),
    Res.
        

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