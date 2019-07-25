-module(car_service).
-compile(export_all).
-include("entity.hrl").


%%%===================================================================
%%% public functions
%%%===================================================================

sync(Entity) ->
    Res = sync_repository:add(Entity),
    utils:log("SYNC ~p", [Res]),
    sync_marshalling(Res).


adj(Entity) ->
    [Front, Rear] = if Entity#adj_entity.side == -1 -> 
        get_left(Entity);
    Entity#adj_entity.side == 1 ->
        get_right(Entity)
    end,    
    adj_repository:add(Entity),
    if Entity#adj_entity.state == stop ->
        io:format("Adj delete stopped car"),
        adj_repository:delete(Entity),
        sync_repository:delete(#sync_entity{name = Entity#adj_entity.name, side = Entity#adj_entity.side, power = Entity#adj_entity.power});
    true ->
        ok 
    end,
    [adj_marshalling(Front), adj_marshalling(Rear)].


last_adj(Side) ->
    Adj = adj_repository:get_all(),
    Last = if Side == -1 -> 
        utils:first_element(Adj);
    true ->
        utils:last_element(Adj)
    end,    
    last_adj_marshalling(Last, Side).
        

kill(Name, Target) ->
    utils:log("Kill ~p", [Target]),
    SelectedCars = adj_repository:select(Target),
    utils:log("Kill ~p", [SelectedCars]),
    if length(SelectedCars) == 1 -> 
        [SelectedCar] = SelectedCars,

        adj_repository:delete(SelectedCar),
        sync_repository:delete(#sync_entity{name = SelectedCar#adj_entity.name, side = SelectedCar#adj_entity.side, power = SelectedCar#adj_entity.power}),
        
        utils:log("Selected Car: ~p", [SelectedCar]),
        Hosts = host_repository:select(SelectedCar),

        utils:log("Selected Hosts: ~p", [Hosts]),
        if length(Hosts) == 1 -> 
            [Host] = Hosts,
            utils:kill_car(Host, SelectedCar);   
        true ->
            host_undefined
        end;
    true ->
        car_undefined
    end,
    SelectedCars2 = adj_repository:select(Name),
    
    if length(SelectedCars2) == 1 -> 
        utils:log("Killer caller was not in sync state"),
        [AdjCaller] = SelectedCars2,
        adj(AdjCaller);
    true ->
        SelectedCars3 = sync_repository:select(Name),
        if length(SelectedCars3) == 1 -> 
            utils:log("Killer caller was in sync state"),
            [SyncCaller] = SelectedCars3,
            sync_to_adj(sync_repository:add(SyncCaller));
        true ->
            [ [], [] ]
        end
    end.    


sync_to_adj([]) ->
    [ [], [] ];
sync_to_adj(First) ->
    [ adj_marshalling( [ #adj_entity{
                                      name = First#sync_entity.name,
                                      side = First#sync_entity.side,
                                      power = First#sync_entity.power
                                    } ] ), [] ].

%%%===================================================================
%%% private functions
%%%===================================================================
        
get_left(Entity) ->
    [Left , Right] = split(adj_repository:get_all(), Entity#adj_entity.name, left),
    [ utils:first_elements(Right, Entity#adj_entity.power), lists:reverse(utils:last_elements(Left, Entity#adj_entity.power)) ].


get_right(Entity) ->
    [Left , Right] = split(adj_repository:get_all(), Entity#adj_entity.name, right),
    [ lists:reverse(utils:last_elements(Left, Entity#adj_entity.power)), utils:first_elements(Right, Entity#adj_entity.power) ].
    
                                        
split(List, Name, Side) ->
    ItemIndex = get_index(List, Name, 0),
    [split_left(List, ItemIndex, Side), split_right(List, ItemIndex, Side)].


split_left(List, -1, right) ->
    List;
split_left(_List, -1, left) ->
    [];
split_left(_List, 0, _Side) ->
    [];
split_left([First | Rest], ItemIndex, Side) ->
    [First | split_left(Rest, ItemIndex - 1, Side)].


split_right(List, ItemIndex, Side) -> 
    RightChunk = split_right_wrapper(List, ItemIndex - 1, Side),
    if length(RightChunk) > 0, ItemIndex =/= -1 -> 
        [_First | Rest] = RightChunk,
        Rest;
    true -> 
        RightChunk
    end.


split_right_wrapper(_List, -2, right) ->
    [];
split_right_wrapper(List, -2, left) ->
    List;
split_right_wrapper([], _ItemIndex, _Side) ->
    [];
split_right_wrapper([First | Rest], ItemIndex, Side) ->
    if ItemIndex > -1 -> 
        split_right_wrapper(Rest, ItemIndex - 1, Side);
    true ->
        [ First | split_right_wrapper(Rest, ItemIndex, Side) ]
    end.


get_index([], _Name, _Counter) ->
    -1;
get_index([First | Rest], Name, Counter) ->
    if First#adj_entity.name == Name ->
        Counter;
    true ->
        get_index(Rest, Name, Counter + 1)
    end.