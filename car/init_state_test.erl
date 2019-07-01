-module(init_state_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("car.hrl"). 

% erl -sname car1@car1 -run init_state_test init_test_
% init_state_test:init_test_().

sync_test_() ->

    register(supervisor, self()),    
    Env = utils:load_environment(),
    
    State = #carState{
                        name = car1, 
                        side = -1, 
                        power = 2, 
                        speed = 0,
                        crossing = false,
                        adj = #adj{frontCars = [#carState{
                            name = 2,
                            side = 1,
                            power = 2
                        }], rearCars = []}, 
                        arrivalTime = utils:getTimeStamp(), 
                        state = init,
                        turn = 1000,
                        bridgeCapacity = 5, 
                        bridgeLength = 3,
                        maxSpeed = 2,
                        towTruckTime = 1500
                    },

    car:start_link(State#carState.name, State),
    Response = car:defaultBehaviour(State#carState.name),
    Response1 = car:defaultBehaviour(State#carState.name),

    io:format("asdf ~n ~p ~n", [Response]),
    io:format("asdf ~n ~p ~n", [Response1]),

    receive
        {check, Name, Target} ->
            utils:log("Supervisor: check request"),
            car:check(Name, updateCurrentTime(Target))
    end.

    %[   ?_assert() ].