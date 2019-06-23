-module(car).
-behaviour(gen_statem).
-compile(export_all).
-define(SERVER, ?MODULE).
-record (state, {arrivalTime, timeout}).


%%%===================================================================
%%% API
%%%===================================================================
 
start_link() ->
        gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).
 
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

init([]) ->
    {ok, create, #state{arrivalTime=1, timeout=0}}.

create({call, From}, Event, Data) ->
    case Event of
        sync ->
            io:format("car is synchronized~n"),
    	    io:format("Car is waiting in the queue.~n"),
            {next_state, coda, Data, [{reply, From, "synchronized"}]};
	crash ->
            io:format("car is dead~n"),
            {next_state, dead, #state{arrivalTime=Data#state.arrivalTime, timeout=5000}, [{reply, From, "dead"}]}   
    end.

coda({call, From}, Event, Data) ->
    case Event of
        move ->
	    io:format("car is crossing~n"),
            {next_state, crossing, #state{arrivalTime=Data#state.arrivalTime, timeout=10000}, [{reply, From, "the car is crossing"}]};
	crash ->
            io:format("car is dead~n"),
            {next_state, dead, #state{arrivalTime=Data#state.arrivalTime, timeout=5000}, [{reply, From, "dead"}]};
	newleader ->
            io:format("car is the new leader~n"),
            {next_state, leader, Data, [{reply, From, "leader"}]}
    end.

leader({call, From}, Event, Data) ->
    case Event of
        move ->
	    io:format("car is crossing~n"),
            {next_state, crossing, #state{arrivalTime=Data#state.arrivalTime, timeout=10000}, [{reply, From, "the car is crossing"}]};
	crash ->
            io:format("car is dead~n"),
            {next_state, dead, #state{arrivalTime=Data#state.arrivalTime, timeout=5000}, [{reply, From, "dead"}]}  
    end.

crossing({call, From}, Event, Data) ->
    case Event of
	crash ->
            io:format("car is dead~n"),
            {next_state, dead, #state{arrivalTime=Data#state.arrivalTime, timeout=5000}, [{reply, From, "dead"}]};
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


