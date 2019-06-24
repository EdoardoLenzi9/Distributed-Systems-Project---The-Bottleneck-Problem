-module(car).
-behaviour(gen_statem).
-compile(export_all).
-define(SERVER, ?MODULE).
-include("car.hrl"). 

%%%===================================================================
%%% API
%%%===================================================================
name() -> ?SERVER.

bs() -> 
    start_link().

start_link(Timeout) ->
    gen_statem:start_link({global, name()}, ?MODULE, [Timeout], []).

start_link() ->
    gen_statem:start_link({global, name()}, ?MODULE, [], []).
 
stop() ->
        gen_statem:stop({global, name()}).

sync() ->
        gen_statem:call({global, name()}, sync).
 
crash() ->
        gen_statem:call({global, name()}, crash).

move() ->
        gen_statem:call({global, name()}, move).

newleader() ->
        gen_statem:call({global, name()}, newleader).

callback_mode()-> state_functions.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

killer(Timeout) ->
    timer:apply_after(Timeout, gen_statem, call, [{global, name()}, crash]).


launcher(Event) ->
    io:format("launcher: call ~p~n", [Event]),
    try gen_statem:call({global, name()}, Event) of 
        _ -> { } 
    catch 
        exit:_ -> {launcher(Event)}; 
        error:_ -> {launcher(Event)};
        throw:_ -> {launcher(Event)} 
    end. 

launchEvent(Handler, [Args]) -> 
    spawn(?MODULE, Handler, [Args]);
launchEvent(Handler, Args) -> 
    spawn(?MODULE, Handler, Args).

init([Timeout]) ->
    Adj = #adj{frontCars = [], rearCars = []},
    launchEvent(killer, [Timeout]),
    launchEvent(launcher, init),
    {ok, create, #carState{adj = Adj, arrivalTime=1, timeout=0}};
init([]) ->
    io:format("~p", [?MODULE]),
    Adj = #adj{frontCars = [], rearCars = []},
    launchEvent(launcher, init),
    {ok, create, #carState{adj = Adj, arrivalTime=1, timeout=0}}.
        

updateTimeout(Data, Timeout) ->
    #carState{ arrivalTime = Data#carState.arrivalTime, 
            name = Data#carState.name,
            delta = Data#carState.delta,
            adj = Data#carState.adj,
            timeout=Timeout }.


updateAdj(Data, Adj) ->
    #carState{ arrivalTime = Data#carState.arrivalTime, 
            name = Data#carState.name,
            delta = Data#carState.delta,
            adj = Adj,
            timeout=Data#carState.timeout }.


updateDelta(Data, Delta) ->
    #carState{ arrivalTime = Data#carState.arrivalTime, 
            name = Data#carState.name,
            delta = Delta,
            adj = Data#carState.adj,
            timeout=Data#carState.timeout }.


lastElement(List) ->
    [Pivot] = lists:nthtail(length(List)-1, List),
    Pivot.


firstElement(List) ->
    [First | Rest] = List,
    First.


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




create({call, From}, Event, Data) ->
    case Event of
        init ->
            if Data#carState.adj#adj.frontCars =/= [] ->
                Pivot = lastElement(Data#carState.adj#adj.frontCars),
                {next_state, coda, updateDelta(Data, berkeley(Pivot)), [{reply, From, "sync completed"}]};   
            true -> 
                launchEvent(launcher, defaultLeaderBehaviour),
                {next_state, leader, Data, [{reply, From, "leader"}]}
            end;
        sync ->
            no_sync;
	    crash ->
            io:format("car is dead~n"),
            {next_state, dead, updateTimeout(Data, 5000), [{reply, From, "dead"}]}   
    end.


coda({call, From}, Event, Data) ->
    case Event of
        move ->
	    io:format("car is crossing~n"),
            {next_state, crossing, updateTimeout(Data, 10000), [{reply, From, "the car is crossing"}]};
	    crash ->
            io:format("car is dead~n"),
            {next_state, dead, updateTimeout(Data, 5000), [{reply, From, "dead"}]};
	    newleader ->
            io:format("car is the new leader~n"),
            {next_state, leader, Data, [{reply, From, "leader"}]}
    end.


leader({call, From}, Event, Data) ->
    case Event of
        {newCar, front, NewCar} -> 
            NewData = updateAdj(Data, Data#carState.adj#adj.frontCars ++ [NewCar]);
        {newCar, rear, NewCar} -> 
            NewData = updateAdj(Data, [NewCar | Data#carState.adj#adj.frontCars]);
        defaultLeaderBehaviour ->
                        
        move ->
	        io:format("car is crossing~n"),
            {next_state, crossing, updateTimeout(Data, 10000), [{reply, From, "the car is crossing"}]};
	    crash ->
            io:format("car is dead~n"),
            {next_state, dead, updateTimeout(Data, 5000), [{reply, From, "dead"}]}  
    end.


crossing({call, From}, Event, Data) ->
    case Event of
	crash ->
            io:format("car is dead~n"),
            {next_state, dead, updateTimeout(Data, 5000), [{reply, From, "dead"}]};
	timeout ->
	    io:format("car crossed the bridge~n"),
	    stop()
    end.
    

dead({call, From}, Event, Data) ->
    case Event of
   	timeout ->
	   io:format("the car has been removed~n"),
	   stop()
    end.
 

terminate(_Reason, _StateName, _State) ->
    ok.
 

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.