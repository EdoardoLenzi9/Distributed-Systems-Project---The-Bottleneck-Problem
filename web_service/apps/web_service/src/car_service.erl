-module(car_service).
-compile(export_all).
-include("entity.hrl").


%%%===================================================================
%%% public functions
%%%===================================================================

sync(Entity) ->
    Sync = if Entity#syncEntity.side == -1 -> 
        utils:first_elements( sync_repository:get_all(), Entity#syncEntity.power);
    Entity#syncEntity.side == 1 ->
        lists:reverse(utils:last_elements( sync_repository:get_all(), Entity#syncEntity.power))
    end,    
    sync_repository:add(Entity),
    sync_marshalling(Sync).


adj(Entity) ->
    [Front, Rear] = if Entity#adjEntity.side == -1 -> 
        get_left(Entity);
    Entity#adjEntity.side == 1 ->
        get_right(Entity)
    end,    
    adj_repository:add(Entity),
    [adj_marshalling(Front), adj_marshalling(Rear)].


%%%===================================================================
%%% private functions
%%%===================================================================
        
get_left(Entity) ->
    [Left , Right] = split(adj_repository:get_all(), Entity#adjEntity.name, left),
    [ utils:first_elements(Right, Entity#adjEntity.power), lists:reverse(utils:last_elements(Left, Entity#adjEntity.power)) ].


get_right(Entity) ->
    [Left , Right] = split(adj_repository:get_all(), Entity#adjEntity.name, right),
    [ lists:reverse(utils:last_elements(Left, Entity#adjEntity.power)), utils:first_elements(Right, Entity#adjEntity.power) ].
    
                                        
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
    if First#adjEntity.name == Name ->
        Counter;
    true ->
        get_index(Rest, Name, Counter + 1)
    end.