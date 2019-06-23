-module(carsfsm).
-behaviour(gen_fsm).
-compile(export_all).
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
 
start_link() ->
        gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).
 
stop() ->
        gen_fsm:send_all_state_event(?SERVER, stop).

sync() ->
        gen_fsm:send_event(?SERVER, sync).
 
crash() ->
        gen_fsm:send_event(?SERVER, crash).

move() ->
        gen_fsm:send_event(?SERVER, move).

newleader() ->
        gen_fsm:send_event(?SERVER, newleader).
%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
 
init([]) ->
    {ok, create, [], 0}.

create(Event, State) ->
    case Event of
        sync ->
            io:format("car is synchronized~n"),
    	    io:format("Car is waiting in the queue.~n"),
            {next_state, coda, State};
	crash ->
            io:format("car is dead~n"),
            {next_state, dead, State, 5000};
	_ ->
	   io:format("cannot understand~n"),
	   {next_state, create, State}   
    end.

coda(Event, State) ->
    case Event of
        move ->
	    io:format("car is crossing~n"),
            {next_state, crossing, State, 10000};
	crash ->
            io:format("car is dead~n"),
            {next_state, dead, State, 5000};
	newleader ->
            io:format("car is the new leader~n"),
            {next_state, leader, State};
	_ ->
	   io:format("cannot understand~n"),
	   {next_state, coda, State} 
    end.

leader(Event, State) ->
    case Event of
        move ->
	    io:format("car is crossing~n"),
            {next_state, crossing, State, 10000};
	crash ->
            io:format("car is dead~n"),
            {next_state, dead, State, 5000};
	_ ->
	   io:format("cannot understand~n"),
	   {next_state, leader, State}  
    end.

crossing(Event, State) ->
    case Event of
	crash ->
            io:format("car is dead~n"),
            {next_state, dead, State, 5000};
	timeout ->
	  io:format("car crossed the bridge~n"),
	   handle_event(stop, dead, State);
	_ ->
	   io:format("cannot understand~n"),
	   {next_state, crossing, State,0}
    end.

dead(Event, State) ->
    case Event of
   	timeout ->
	   io:format("the car has been removed~n"),
	   handle_event(stop, dead, State);
	_ ->
	   io:format("nothing to do, the car is dead~n"),
	   {next_state, dead, State}
    end.

handle_event(stop, _StateName, State) ->
        {stop, normal, State};
handle_event(_Event, StateName, State) ->
        {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.
 
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.
 
terminate(_Reason, _StateName, _State) ->
    ok.
 
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


