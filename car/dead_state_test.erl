%%%===================================================================
%%% Test for the leader state
%%%===================================================================


-module(dead_state_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("car.hrl"). 


% Launch every test with
% cerl ; erl -sname car1 -setcookie ds-project -run dead_state_test test


% cerl ; erl -sname car1 -setcookie ds-project -run dead_state_test dead_test_
dead_test_() ->
    % Arrange
    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync(State),
    test_fixture:skip_normal(State),
    test_fixture:skip_leader(State),
    test_fixture:skip_next(),

    % Act and Assert
    test_fixture:skip_next(),
    test_fixture:listen(stop, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:stop(ReqSender)
    end).


% cerl ; erl -sname car1 -setcookie ds-project -run dead_state_test dead2_test_
dead2_test_() ->
    % Arrange
    test_fixture:register(),
    State = test_fixture:default_state2(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync2(State),
    test_fixture:skip_normal2(State),
    test_fixture:skip_leader2(State),
    test_fixture:skip_next(),

    % Act and Assert
    test_fixture:skip_next(),
    test_fixture:listen(stop, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:stop(ReqSender)
    end).


% cerl ; erl -sname car1 -setcookie ds-project -run dead_state_test dead3_test_
dead3_test_() ->
    % Arrange
    test_fixture:register(),
    State = test_fixture:default_state3(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync3(State),
    test_fixture:skip_normal3(State),
    test_fixture:skip_leader3(State),
    test_fixture:skip_next(),
    
    % Act and Assert
    test_fixture:skip_next(),    
    test_fixture:listen(update_rear, fun(ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody) -> 
        flow:launch_event(request_timer, [{ReqLabel, ReqSender, ReqTarget, utils:get_timestamp(), ReqRTT, ReqBody}])
    end),
    test_fixture:listen(stop, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:stop(ReqSender)
    end),
    test_fixture:listen(update_rear, fun(_ReqLabel, ReqSender, ReqTarget, ReqNickname, ReqSendingTime, _ReqBody) -> 
        utils:log("update rear reached")
    end).


% cerl ; erl -sname car1 -setcookie ds-project -run dead_state_test dead_state_1_test_
dead_state_1_test_() ->
    % Arrange
    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync(State),
    test_fixture:skip_normal(State),
    test_fixture:skip_leader(State),

    test_fixture:listen(next, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:crash(ReqSender, 1)
    end),

    test_fixture:skip_next(),
    test_fixture:listen(call_tow_truck, fun(_ReqLabel, _ReqSender, ReqTarget, _ReqRTT, ReqBody) -> 
        flow:launch_event(tow_truck, [ReqBody, ReqTarget])
    end),
    test_fixture:listen(tow_truck, fun(_ReqLabel, _ReqSender, ReqTarget, _ReqRTT, _ReqBody) -> 
        car:tow_truck(ReqTarget)
    end),
    test_fixture:listen(stop, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:stop(ReqSender)
    end).


% cerl ; erl -sname car1 -setcookie ds-project -run dead_state_test dead_state_2_test_
dead_state_2_test_() ->
    % Arrange
    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync(State),
    test_fixture:skip_normal(State),
    test_fixture:skip_leader(State),

    test_fixture:listen(next, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:crash(ReqSender, 2)
    end),
    test_fixture:listen(next, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:default_behaviour(ReqSender),
        flow:launch_event(tow_truck, [State#car_state.tow_truck_time, ReqSender])
    end),
    test_fixture:listen(tow_truck, fun(_ReqLabel, _ReqSender, ReqTarget, _ReqRTT, _ReqBody) -> 
        car:tow_truck(ReqTarget)
    end),
    test_fixture:listen(stop, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:stop(ReqSender)
    end).