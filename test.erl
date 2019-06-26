-module(test).
-compile(export_all).

start() ->
    A = [1,2,3,4,5,6],
    firstElements([], 3).

firstElements([ ], _) ->
    [ ];
firstElements([First | Rest], Hop) ->
    if Hop > 0 -> 
        [First | firstElements(Rest, Hop - 1)];
    true -> 
        [ ]
    end.