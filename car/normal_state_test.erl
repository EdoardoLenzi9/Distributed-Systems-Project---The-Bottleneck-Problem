%%%===================================================================
%%% Test for the normal state
%%%===================================================================


-module(normal_state_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("car.hrl"). 


% Launch every test with
% erl -sname car1 -setcookie ds-project -run normal_state_test test
  
% cerl ; erl -sname car1 -setcookie ds-project -run normal_state_test normal_test_
normal_test_() ->
    % Arrange 

    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State#car_state{ delta = 0 }),
    test_fixture:skip_sync(State),

    % Act and Assert
    utils:log("Test: car alone"),
    test_fixture:listen(next, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:default_behaviour(ReqSender)
    end),

    car:stop(State#car_state.name).


% cerl ; erl -sname car1 -setcookie ds-project -run normal_state_test normal2_test_
normal2_test_() ->

    test_fixture:register(),
    State = test_fixture:default_state2(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync2(State),

    test_fixture:listen(next, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:default_behaviour(ReqSender)
    end),
    test_fixture:listen(check, fun(_ReqLabel, _ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        utils:log("Test: Car2 remains in the same position -1.5"),
        car:check_reply({node(), node(), utils:get_timestamp(), 0, test_fixture:queue_car(-1.5, false)})
    end),
    test_fixture:skip_log_state(),
    test_fixture:listen(wait, fun(ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody) -> 
        flow:launch_event(timer, [{ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody}])
    end),
    test_fixture:skip_log_state(),
    test_fixture:listen(wait_reply, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:default_behaviour(ReqSender)
    end),
    test_fixture:listen(check, fun(_ReqLabel, _ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        utils:log("Test: Car2 moves to position 0"),
        car:check_reply({node(), node(), utils:get_timestamp(), 0, test_fixture:queue_car(-0.5, false)})
    end),
    test_fixture:skip_log_state(),
    test_fixture:listen(wait, fun(ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody) -> 
        flow:launch_event(timer, [{ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody}])
    end),
    test_fixture:skip_log_state(),
    test_fixture:listen(wait_reply, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:default_behaviour(ReqSender)
    end),
    test_fixture:listen(check, fun(_ReqLabel, _ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        utils:log("Test: Car2 moves to crossing position -1"),
        car:check_reply({node(), node(), utils:get_timestamp(), 0, test_fixture:queue_car(-1.5, true)})
    end),
    test_fixture:skip_log_state(),
    test_fixture:listen(wait, fun(ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody) -> 
        flow:launch_event(timer, [{ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody}])
    end),
    test_fixture:skip_log_state(),
    test_fixture:listen(wait_reply, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:default_behaviour(ReqSender)
    end),
    test_fixture:listen(check, fun(_ReqLabel, _ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        utils:log("Test: Car2 moves to crossing position 0"),
        car:check_reply({node(), node(), utils:get_timestamp(), 0, test_fixture:queue_car(-0.5, true)}),
        car:update_front(_ReqSender, [])
    end),
    test_fixture:skip_log_state(),
    test_fixture:listen(wait, fun(ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody) -> 
        flow:launch_event(timer, [{ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody}])
    end),
    test_fixture:skip_log_state(),
    test_fixture:listen(wait_reply, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:default_behaviour(ReqSender)
    end),
    test_fixture:listen(check, fun(_ReqLabel, _ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        utils:log("Test: receive fail check")
    end),
    test_fixture:skip_log_state(),
    test_fixture:listen(default_behaviour, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:default_behaviour(ReqSender)
    end),
    test_fixture:listen(wait, fun(ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody) -> 
        flow:launch_event(timer, [{ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody}])
    end),
    test_fixture:skip_log_state(),
    test_fixture:listen(wait_reply, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:default_behaviour(ReqSender)
    end),
    car:stop(State#car_state.name).


% cerl ; erl -sname car1 -setcookie ds-project -run normal_state_test normal3_test_
normal3_test_() ->
    % Arrange 

    test_fixture:register(),
    State = test_fixture:default_state3(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync3(State),

    % Act and Assert

    test_fixture:listen(next, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:default_behaviour(ReqSender)
    end),

    car:stop(State#car_state.name).


% cerl ; erl -sname car1 -setcookie ds-project -run normal_state_test normal_crash_test_
normal_crash_test_() ->
    % Arrange 

    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State#car_state{ delta = 0 }),
    test_fixture:skip_sync(State),

    % Act and Assert

    test_fixture:listen(next, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
            car:default_behaviour(ReqSender)
    end),
    test_fixture:listen(next, fun(_ReqLabel, _ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        ok
    end),
    utils:log("Test: launch crash"),
    flow:launch_event(killer, [State#car_state.name, 0]),
    test_fixture:listen(crash, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:crash(ReqSender, 2)
    end),

    car:stop(State#car_state.name).