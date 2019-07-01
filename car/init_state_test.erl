%%%===================================================================
%%% Test for the init state
%%%===================================================================


-module(init_state_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("car.hrl"). 

% erl -sname car1@car1 -run init_state_test test
% erl -sname car1@car1 -run init_state_test sync_test_
% init_state_test:sync_test_().

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
                        arrivalTime = utils:get_timestamp(), 
                        state = init,
                        turn = 1000,
                        bridgeCapacity = 5, 
                        bridgeLength = 3,
                        maxSpeed = Env#env.maxSpeed,
                        tow_truckTime = Env#env.tow_truckTime,
                        maxRTT = Env#env.maxRTT
                    },

    car:start_link(State#carState.name, State),
    SyncResponse = car:default_behaviour(State#carState.name),
    ExpectedSyncResponse = sync_default_behaviour,
    
    Assertion2 = receive
        {check, Name, Target} ->
            {Response2, Delta} = car:check(Name, update_current_time(Target)),
            ExpectedResponse2 = sync_response_check,
            assert(Response2, ExpectedResponse2)
    end,
    utils:log("~p", [Assertion2]),
    utils:log("Supervisor call response: ~p", [car:default_behaviour(State#carState.name)]),

    [ ?_assert(SyncResponse =:= ExpectedSyncResponse) ].


assert(CurrentResult, ExpectedResult) ->
    if CurrentResult == ExpectedResult ->
        ok;
    true -> 
        throw("test fail")
    end.