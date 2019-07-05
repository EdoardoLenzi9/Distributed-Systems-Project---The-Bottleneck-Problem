-module(utils).
-compile(export_all).
-define(LOG, true).


%% Logger
-ifdef(LOG).
    log(String)->
        ParsedString = io:format(String),
        io:format("~n~p~n", [ParsedString]).
    log(String, Args) ->
        ParsedString = io:format(String, Args),
        io:format("~n~p~n", [ParsedString]).
-else.
    log(String)-> ok.
    log(String, Args) -> ok.
-endif.


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


get_timestamp() ->
    {Mega, Seconds, Ms} = os:timestamp(),
    (Mega*1000000 + Seconds)*1000 + erlang:round(Ms/1000).     


concat([]) ->
    [];
concat([First | Rest]) ->
    string:concat(First, concat(Rest)).