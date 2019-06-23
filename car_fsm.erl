-module(car_fsm).
-compile(export_all).

start() ->
	spawn(fun() -> create() end).

crash(Pid) -> Pid ! crash.
synchronized(Pid) -> Pid ! synchronized.
move(Pid) -> Pid ! move.
newleader(Pid) -> Pid ! newleader.

create() ->
    io:format("Car arrived, waiting to synchronize.~n"),
    receive
        crash ->
            io:format("car is dead~n"),
	    dead();
        synchronized ->
            io:format("car is synchronized~n"),
            coda();
	_ ->
	   io:format("cannot understand~n"),
	   create()
    end.

coda() ->
    io:format("Car was added to the queue.~n"),
    receive
        move ->
            crossing();
 	crash ->
            io:format("car is dead~n"),
	    dead();
        newleader ->
            io:format("car is the new leader~n"),
            leader();
	_ ->
	   io:format("cannot understand~n"),
	   coda()
    end.

leader() ->
   receive
	crash ->
            io:format("car is dead~n"),
	    dead();
	move ->
            crossing();
	_ ->
	   io:format("cannot understand~n"),
	   leader()
   end.	

crossing() ->
    io:format("car is crossing~n"),
    receive
        crash ->
            io:format("car is dead~n"),
	    dead()
    after 10000 ->
        io:format("car crossed the bridge~n")
    end.

dead() ->
   receive
	_ ->
	   io:format("nothing to do, the car is dead~n"),
	   dead()
   after 20000 ->
	io:format("the car has been removed~n")
   end.
