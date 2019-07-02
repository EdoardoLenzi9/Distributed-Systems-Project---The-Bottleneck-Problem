%%%===================================================================
%%% Test for the init state
%%%===================================================================


-module(normal_state_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("car.hrl"). 

% erl -sname car1@car1 -run normal_state_test test
% erl -sname car1@car1 -run normal_state_test normal_test_
% normal_state_test:normal_test_().
  

normal_alone_test_() ->

    register(supervisor, self()),    
    Env = utils:load_environment(),
    
    State = #car_state{
                        name = car1, 
                        side = -1, 
                        power = 2, 
                        speed = 0,
                        crossing = false,
                        adj = #adj{front_cars = [], rear_cars = []}, 
                        arrival_time = utils:get_timestamp(), 
                        delta = 40,
                        state = normal,
                        turn = 1000,
                        bridge_capacity = 5, 
                        bridge_length = 3,
                        max_speed = Env#env.max_speed,
                        tow_truck_time = Env#env.tow_truck_time,
                        max_RTT = Env#env.max_RTT
                    },

    car:start_link(State#car_state.name, State),
    SyncResponse = car:default_behaviour(State#car_state.name),
    ExpectedSyncResponse = sync_default_behaviour,
    
    Assertion2 = receive
        {check, Name, Target} ->
            {Response2, _Delta} = car:check(Name, Target#car_state{current_time = utils:get_timestamp()}),
            ExpectedResponse2 = sync_response_check,
            assert(Response2, ExpectedResponse2)
    end,
    utils:log("~p", [Assertion2]),
    utils:log("Supervisor call response: ~p", [car:default_behaviour(State#car_state.name)]),

    [ ?_assert(SyncResponse =:= ExpectedSyncResponse) ].


assert(CurrentResult, ExpectedResult) ->
    if CurrentResult == ExpectedResult ->
        ok;
    true -> 
        throw("test fail")
    end.