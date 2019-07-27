%%%===================================================================
%%% Test for the leader state
%%%===================================================================


-module(leader_state_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("car.hrl"). 


% Launch every test with
% cerl ; erl -sname car1 -setcookie ds-project -run leader_state_test test
  

% cerl ; erl -sname car1 -setcookie ds-project -run leader_state_test leader_test_
leader_test_() ->
    % Arrange

    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync(State),
    test_fixture:skip_normal(State),

    % Act and Assert

    test_fixture:listen(next, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:default_behaviour(ReqSender)
    end),
    
    car:stop(State#car_state.name).


% cerl ; erl -sname car1 -setcookie ds-project -run leader_state_test leader2_test_
leader2_test_() ->
    % Arrange

    test_fixture:register(),
    State = test_fixture:default_state3(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync2(State),
    test_fixture:skip_normal2(State),

    % Act and Assert

    test_fixture:listen(next, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:default_behaviour(ReqSender)
    end),

    car:stop(State#car_state.name).
    

% cerl ; erl -sname car1 -setcookie ds-project -run leader_state_test leader_test3_
leader_test3_() ->
    % Arrange
    test_fixture:register(),
    State = test_fixture:default_state3(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync3(State),
    test_fixture:skip_normal3(State),

    % Act and Assert

    test_fixture:listen(next, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:default_behaviour(ReqSender)
    end),
    test_fixture:listen(check, fun(ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody) -> 
        flow:launch_event(request_timer, [{ReqLabel, ReqSender, ReqTarget, utils:get_timestamp(), ReqRTT, ReqBody}])   
    end),
    test_fixture:listen(check, fun(_ReqLabel, ReqSender, ReqTarget, ReqNickname, ReqSendingTime, _ReqBody) -> 
        {_Result, Data} = car:check(ReqTarget),
        supervisor_reply_supervisor_api:sup_reply({check_reply, ReqTarget, ReqSender, ReqNickname, ReqSendingTime, Data})
    end),
    test_fixture:listen(check_reply, fun(_ReplyLabel, ReplySender, ReplyTarget, ReplySendingTime, ReplyBody) -> 
        RTT = utils:get_timestamp() - ReplySendingTime,
        {_Result, _Data} = car:check_reply({ReplySender, ReplyTarget, ReplySendingTime, RTT, ReplyBody#car_state{arrival_time = ReplySendingTime}})
    end),
    car:stop(State#car_state.name).
