-module(car_service).
-compile(export_all).
-include("entity.hrl").


%%%===================================================================
%%% public functions
%%%===================================================================

sync(Entity) ->
    Sync = if Entity#sync_entity.side == -1 -> 
        utils:first_elements( sync_repository:get_all(), Entity#sync_entity.power);
    Entity#sync_entity.side == 1 ->
        lists:reverse(utils:last_elements( sync_repository:get_all(), Entity#sync_entity.power))
    end,    
    sync_repository:add(Entity),
    sync_marshalling(Sync).


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