%%%===================================================================
%%% Car supervisor interface
%%%===================================================================

-module(car_supervisor).
-export([start/1]).


start(Name) ->
    io:format("~p", [Name]),
    register(Name, self()),
    car:bs(),
    receive
        % TODO update api
        crash ->
            car:crash(Name); 
        stop ->
            car:stop(Name); 
        sync ->
            car:sync(Name); 
        newleader ->
            car:newleader(Name)
    end.