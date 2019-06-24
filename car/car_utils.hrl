%%%===================================================================
%%% generic utility functions, environment
%%%===================================================================
-define(LOG, true).


%% Logger
-ifdef(LOG).
    log(String)->
        io:printf(String).
    log(String, Args) ->
        io:printf(String, Args).
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
    try gen_statem:call({global, ?MODULE}, init) of 
        _ -> { } 
    catch 
        exit:_ -> {launcher(Event)}; 
        error:_ -> {launcher(Event)};
        throw:_ -> {launcher(Event)} 
    end. 


%%%===================================================================
%%% list management
%%%===================================================================

% [1,2,3] -> 3
lastElement(List) ->
        [Pivot] = lists:nthtail(length(List)-1, List),
        Pivot.
    

% [1,2,3] -> 1    
firstElement(List) ->
    [First | _ ] = List,
    First.


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