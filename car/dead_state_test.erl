%%%===================================================================
%%% Test for the leader state
%%%===================================================================


-module(dead_state_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("car.hrl"). 


% Launch every test with
% erl -sname car1@car1 -run dead_state_test test
  

%cerl ; erl -sname car1@car1 -run dead_state_test dead1_test_
dead1_test_() ->
    % Arrange
    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_to_dead(State, 1),
    
    receive
        {car_call, Req1} ->
            {Label1, Sender1, _Target1, _RTT1, _Body1} = Req1,
            case Label1 of 
                next ->
                    utils:log("Test: receive next call"),
                    car:default_behaviour(Sender1)
            end
    end,
    receive
        {car_call, Req2} ->
            utils:log("Test: receive call_tow_truck"),
            {Label2, Sender2, Target2, RTT2, Body2} = Req2,
            flow:launch_event(tow_truck, [Body2, Target2])
    end,
    receive
        {car_call, Req3} ->
            utils:log("Test: receive tow_truck"),
            {Label3, Sender3, Target3, RTT3, Body3} = Req3,
            utils:log("Test: launch dead tow_truck"),
            car:tow_truck(Target3)
    end,
    receive
        {car_call, Req4} ->
            utils:log("Test: receive stop"),
            {Label4, Sender4, Target4, RTT4, Body4} = Req4,
            utils:log("Test: launch dead stop"),
            car:stop(Sender4)
    end.
    % Act 