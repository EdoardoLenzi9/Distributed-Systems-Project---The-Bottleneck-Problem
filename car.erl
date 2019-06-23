-module(car).
-behaviour(gen_statem).
-compile(export_all).
-define(SERVER, ?MODULE).
-include("car.hrl"). 

%%%===================================================================
%%% API
%%%===================================================================
 
start_link(Timeout, Name="dodo") ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [Name, Timeout], []).

start_link(Name = "dodo") ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [Name], []).
 
stop() ->
        gen_statem:stop(?SERVER).

sync() ->
        gen_statem:call(?SERVER, sync).
 
crash() ->
        gen_statem:call(?SERVER, crash).

move() ->
        gen_statem:call(?SERVER, move).

newleader() ->
        gen_statem:call(?SERVER, newleader).

callback_mode()-> state_functions.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

killer(Timeout, ParentName) ->
    %%timer:apply_after(Timeout, os, cmd, ["gedit &"]).
    timer:apply_after(Timeout, gen_statem, call, [ParentName, crash]).

    init([Name, Timeout]) ->
        spawn(?MODULE, killer, [Timeout, Name]),
        {ok, create, #state{arrivalTime=1, name = Name, timeout=0}};
    init([Name]) ->
        {ok, create, #state{arrivalTime=1, name = Name, timeout=0}}.
        

updateTimeout(Data, Timeout) ->
    #state{ arrivalTime = Data#state.arrivalTime, 
            name = Data#state.name,
            delta = Data#state.delta,
            adj = Data#state.adj,
            timeout=Timeout }.


updateAdj(Data, Adj) ->
    #state{ arrivalTime = Data#state.arrivalTime, 
            name = Data#state.name,
            delta = Data#state.delta,
            adj = Adj,
            timeout=Data#state.timeout }.


updateDelta(Data, Delta) ->
    #state{ arrivalTime = Data#state.arrivalTime, 
            name = Data#state.name,
            delta = Delta,
            adj = Data#state.adj,
            timeout=Data#state.timeout }.


lastElement(List) ->
    [Pivot] = lists:nthtail(length(List)-1, List),
    Pivot.


getTimeStamp() ->
    {Mega, Seconds, Ms} = os:timestamp(),
    (Mega*1000000 + Seconds)*1000 + erlang:round(Ms/1000).                                                                                                                                              


berkeley(Pivot) ->
    CurrentTime = getTimeStamp(),
    PivotTime = 1561303082320,
    CurrentTime2 = getTimeStamp(),
    RTT = CurrentTime2 - CurrentTime,
    CurrentTime2 - (PivotTime + RTT / 2).


create({call, From}, Event, Data) ->
    Adj = #adj{frontCars = [], rearCars = []},
    %if 
    %    Adj#adj.frontCars == [] ->     % AND
    %        {next_state, leader, updateAdj(Data, Adj), [{reply, From, "synchronized and leader"}]};
    %    true ->
    %        Pivot = lastElement(Data#state.adj#adj.frontCars),
    %        {next_state, coda, updateDelta(Data, berkeley(Pivot)), [{reply, From, "synchronized"}]}
    %end,
    case Event of
        sync ->
            "";
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


