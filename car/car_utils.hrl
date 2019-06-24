%%%===================================================================
%%% generic utility functions, environment
%%%===================================================================
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


%%%===================================================================
%%% spawn support processes
%%%===================================================================

%% Spawn a process that launches an event
launchEvent(Handler, Args) -> 
    log("launchEvent: ~p~p~n", [Handler, Args]),
    spawn(?MODULE, Handler, Args).


%% Simulate a car crash after a given timeout
killer(Timeout) ->
    timer:apply_after(Timeout, gen_statem, call, [{global, ?MODULE}, crash]).
        

%% Launch a given event until success (polling)
launcher(Event) ->
    try gen_statem:call({global, ?MODULE}, Event) of 
        _ -> { } 
    catch 
        exit:_ -> {launcher(Event)}; 
        error:_ -> {launcher(Event)};
        throw:_ -> {launcher(Event)} 
    end. 


next(NextState, Data, From) ->
    log("STATE TRANSITION -> ~p", [NextState]),
    {next_state, NextState, Data, [{reply, From, io:format(NextState)}]}.

keep(Data, From) ->
    log("KEEP STATE"),
    {keep_state, Data, [{reply, From, "keep_state"}]}.
%%%===================================================================
%%% list management
%%%===================================================================


%    Front        Rear
% [A, B, C] CAR [D, E, F]

% [1,2,3] -> 3
lastElement(List) ->
    [Pivot] = lists:nthtail(length(List)-1, List),
    Pivot.
    
lastElement(List, Hop) ->
    [Pivot] = lists:nthtail(length(List)-Hop, List),
    Pivot.
    
% [1,2,3] -> 1    
firstElement([First | _ ]) ->
    First.

firstElement([First | Rest], 1) ->
    First;
firstElement([First | Rest], Hop) ->
    firstElement(Rest, Hop - 1).


%%%===================================================================
%%% time management
%%%===================================================================

getTimeStamp() ->
    {Mega, Seconds, Ms} = os:timestamp(),
    (Mega*1000000 + Seconds)*1000 + erlang:round(Ms/1000).                                                                                                                                              


berkeley(Pivot) ->
    {CurrentTime, PivotTime} = getPivotTime(Pivot),
    CurrentTime2 = getTimeStamp(),
    RTT = CurrentTime2 - CurrentTime,
    CurrentTime2 - (PivotTime + RTT / 2).


getPivotTime(Pivot) -> 
    CurrentTime = getTimeStamp(), 
    Res = gen_statem:call(Pivot, sync),
    if Res == no_sync -> 
        {CurrentTime, getPivotTime(Pivot)};
    true ->
        {CurrentTime, Res}
    end.