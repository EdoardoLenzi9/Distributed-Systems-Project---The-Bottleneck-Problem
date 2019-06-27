-module(adj_repository).
-compile(export_all).
-include("entity.hrl").


add(Name, Side, Power, ArrivalTime, Delta, State) ->
    db_manager:add(#adjEntity{  arrivalTime = ArrivalTime, 
                                name = Name, 
                                side = Side, 
                                power = Power, 
                                delta = Delta, 
                                state = State }).


get_all(Name, Side) ->
    split(order(db_manager:get_all(adjEntity)), Name, Side).


split(List, Name, Side) ->
    ItemIndex = get_index(List, Name, 0),
    [split_left(List, ItemIndex, Side), split_right(List, ItemIndex, Side)].


split_left(List, -1, "left") ->
    List;
split_left(_List, -1, "right") ->
    [];
split_left(_List, 0, _Side) ->
    [];
split_left([First, Rest], ItemIndex, Side) ->
    [First | split_left(Rest, ItemIndex - 1, Side)].


split_right(List, ItemIndex, Side) -> 
    split_right_wrapper(List, ItemIndex -1, Side).


split_right_wrapper(_List, - 2, "left") ->
    [];
split_right_wrapper(List, - 2, "right") ->
    List;
split_right_wrapper([], _ItemIndex, _Side) ->
    [];
split_right_wrapper([First, Rest], ItemIndex, Side) ->
    if ItemIndex > - 1 -> 
        split_right_wrapper(Rest, ItemIndex - 1, Side);
    true ->
        [ First | split_right_wrapper(Rest, ItemIndex - 1, Side) ]
    end.


get_index([], _Name, _Counter) ->
    -1;
get_index([First, Rest], Name, Counter) ->
    if First#adjEntity.name == Name ->
        Counter;
    true ->
        get_index(Rest, Name, Counter + 1)
    end.


get_left(Name, Counter) ->
    [Left , Right] = get_all(Name, "left"),
    [ utils:first_elements(Right, Counter), lists:reverse(utils:last_elements(Left, Counter)) ].


get_right(Name, Counter) ->
    [Left , Right] = get_all(Name, "right"),
    [ lists:reverse(utils:last_elements(Left, Counter)), utils:first_elements(Right, Counter) ].


order(List) ->
    F = fun(X, Y) -> 
        if X#adjEntity.side == "right", Y#adjEntity.side == "right" -> 
            X#adjEntity.arrivalTime + X#adjEntity.delta < Y#adjEntity.arrivalTime + Y#adjEntity.delta;
        X#adjEntity.side == "right", Y#adjEntity.side == "left" ->
            false;
        X#adjEntity.side == "left", Y#adjEntity.side == "right" ->
            true; 
        X#adjEntity.side == "left", Y#adjEntity.side == "left" -> 
            X#adjEntity.arrivalTime + X#adjEntity.delta > Y#adjEntity.arrivalTime + Y#adjEntity.delta
            end
        end,
    lists:sort(F, List).

