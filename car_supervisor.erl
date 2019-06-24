-module(car_supervisor).
-export([start/1]).

start(Name) ->
    io:format("~p", [Name]),
    register(Name, self()),
    car:bs(),
    receive
        crash ->
            car:crash(); 
        stop ->
            car:stop(); 
        sync ->
            car:sync(); 
        move ->
            car:move();
        newleader ->
            car:newleader()
    end.