-module(test).
-compile(export_all).
-record(adjEntity, {         
                        name,
                        side,
                        power,
                        arrivalTime,
                        delta,
                        state
                    }).


start() -> 
    List = [
                #adjEntity{ name = "3", side = "left", arrivalTime = 3, delta = 0.1 },
                #adjEntity{ name = "2", side = "left", arrivalTime = 2, delta = 0.1 },
                #adjEntity{ name = "1", side = "left", arrivalTime = 1, delta = 0.1 },
                #adjEntity{ name = "4", side = "right", arrivalTime = 4, delta = 0.1 },
                #adjEntity{ name = "5", side = "right", arrivalTime = 5, delta = 0.1 },
                #adjEntity{ name = "6", side = "right", arrivalTime = 6, delta = 0.1 }
            ],
            %get_left(List, "3", 3)
            get_right(List, "1", 2).


get_left(List, Name, Counter) ->
    [Left , Right] = get_all(List, Name, "left"),

    io:format("--------~n~n~p~n~n", [Left]),
    io:format("~n~n~p~n~n--------", [Right]),
    
    [ first_elements(Right, Counter), lists:reverse(last_elements(Left, Counter)) ].


get_right(List, Name, Counter) ->
    [Left , Right] = get_all(List, Name, "right"),
    [ lists:reverse(last_elements(Left, Counter)), first_elements(Right, Counter) ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_all(List, Name, Side) ->
    split(order(List), Name, Side).


split(List, Name, Side) ->
    ItemIndex = get_index(List, Name, 0),
    io:format("Index ~p", [ItemIndex] ),
    [split_left(List, ItemIndex, Side), split_right(List, ItemIndex, Side)].


split_left(List, -1, "right") ->
    List;
split_left(_List, -1, "left") ->
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


split_right_wrapper(_List, -2, "right") ->
    [];
split_right_wrapper(List, -2, "left") ->
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


    last_elements(List, Hop) ->
            lists:nthtail(length(List) - erlang:min(length(List), Hop), List).
        
        
        first_elements([ ], _) ->
                [ ];
            first_elements([First | Rest], Hop) ->
                if Hop > 0 -> 
                    [First | first_elements(Rest, Hop - 1)];
                true -> 
                    [ ]
                end.