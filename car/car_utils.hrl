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


%% Spawn a process that launches an event
launchEvent(Handler, [Args]) -> 
    spawn(?MODULE, Handler, [Args]);
launchEvent(Handler, Args) -> 
    spawn(?MODULE, Handler, Args).


%% Simulate a car crash after a given timeout
killer(Timeout) ->
    timer:apply_after(Timeout, gen_statem, call, [{global, ?MODULE}, crash]).
        

%% Launch a given event until success (polling)
launcher(Event) ->
    io:format("launcher: call ~p~n", [Event]),
    try gen_statem:call({global, ?MODULE}, Event) of 
        _ -> { } 
    catch 
        exit:_ -> {launcher(Event)}; 
        error:_ -> {launcher(Event)};
        throw:_ -> {launcher(Event)} 
    end. 
        