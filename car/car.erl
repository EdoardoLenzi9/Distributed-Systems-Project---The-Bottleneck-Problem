-module(car).
-behaviour(gen_statem).

-include("car.hrl"). 
-include("car_std.hrl").
-include("car_api.hrl").
-include("car_utils.hrl").

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================


init([Timeout]) ->
    log("STATE Init - Broken car, Timeout:~p~n", [Timeout]),
    Adj = #adj{frontCars = [], rearCars = []},
    launchEvent(killer, [Timeout]),
    launchEvent(launcher, [init]),
    {ok, create, #carState{adj = Adj, arrivalTime=1, timeout=0}};
init([]) ->
    %log("STATE Init"),
    Adj = #adj{frontCars = [], rearCars = []},
    launchEvent(launcher, [init]),
    %log("STATE TRANSITION: Init -> Create"),
    {ok, create, #carState{adj = Adj, arrivalTime=1, timeout=0}}.
        

create({call, From}, Event, Data) ->
    case Event of
        init ->
            io:format("init here~n"),
            if Data#carState.adj#adj.frontCars =/= [] ->
                Pivot = lastElement(Data#carState.adj#adj.frontCars),
                {next_state, coda, updateDelta(Data, berkeley(Pivot)), [{reply, From, "sync completed"}]};   
            true -> 
                %launchEvent(launcher, defaultLeaderBehaviour),
                {next_state, leader, Data, [{reply, From, "leader"}]}
            end;
        sync ->
            no_sync;
	    crash ->
            io:format("car is dead here~n"),
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
            ok;        
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
    

dead({call, _From}, Event, _Data) ->
    case Event of
   	timeout ->
	   io:format("the car has been removed~n"),
	   stop()
    end.
 
