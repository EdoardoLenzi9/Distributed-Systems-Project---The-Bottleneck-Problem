%%%===================================================================
%%% Test for the normal state
%%%===================================================================


-module(normal_state_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("car.hrl"). 


% Launch every test with
% erl -sname car1@car1 -run normal_state_test test

  
%cerl ; erl -sname car1@car1 -run normal_state_test normal_test_
normal_test_() ->
    % Arrange 

    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State#car_state{ delta = 0 }),
    test_fixture:skip_sync(State),

    % Act
    utils:log("Test: car alone"),
    receive
        {car_call, Req1} ->
            {Label1, Sender1, _Target1, _RTT1, _Body1} = Req1,
            case Label1 of 
                next ->
                    utils:log("Test: receive next call"),
                    car:default_behaviour(Sender1)
                    %assert(Result1, normal_default_behaviour)
            end
    end,
    receive
        {car_call, Req2} ->
            {Label2, _Sender2, _Target2, _RTT2, _Body2} = Req2,
            case Label2 of 
                wait ->
                    utils:log("Test: receive wait call"),
                    flow:launch_event(timer, [Req2])
            end
    end,
    receive
        {car_call, Req3} ->
            {Label3, Sender3, _Target3, _RTT3, _Body3} = Req3,
            case Label3 of 
                % receive turn timeout
                wait_reply ->
                    utils:log("Test: receive wait_reply call"),
                    car:default_behaviour(Sender3)
            end
    end.
    % Assert 
    %[ ?_assert(Response1 =:= ExpectedResponse1) ].


%cerl ; erl -sname car1@car1 -run normal_state_test normal2_test_
normal2_test_() ->

    test_fixture:register(),
    State = test_fixture:default_state2(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync2(State),

    receive
        {car_call, Req1} ->
            {Label1, Sender1, _Target1, _RTT1, _Body1} = Req1,
            case Label1 of 
                % launch normal defaultBehaviour
                next ->
                    utils:log("Test: receive next call"),
                    car:default_behaviour(Sender1)
                    %assert(Result1, normal_default_behaviour)
            end
    end,
    receive
        {car_call, Req2} ->
            {Label2, _Sender2, _Target2, _RTT2, _Body2} = Req2,
            case Label2 of 
                check ->
                    utils:log("Test: receive check call"),
                    utils:log("Test: Car2 remains in the same position -1"),
                    {_Result2, _Data2} = car:check_reply({car2, car1, utils:get_timestamp(), 0, 
                                                           #car_state{  name = car2, 
                                                                        side = State#car_state.side,
                                                                        speed = 0,
                                                                        crossing = false,
                                                                        % remains in the same position -1
                                                                        position = -1,
                                                                        current_time = utils:get_timestamp()}})
            end
    end,
    receive
        {car_call, Req3} ->
            {Label3, _Sender3, _Target3, _RTT3, _Body3} = Req3,
            case Label3 of 
                check ->
                    utils:log("Test: receive check call"),
                    utils:log("Test: Car2 moves to position 0"),
                    {_Result3, _Data3} = car:check_reply({car2, car1, utils:get_timestamp(), 0, 
                                                           #car_state{  name = car2, 
                                                                        side = State#car_state.side,
                                                                        speed = State#car_state.max_speed,
                                                                        crossing = false,
                                                                        % moves to position 0
                                                                        position = 0,
                                                                        current_time = utils:get_timestamp()}})
            end
    end.


%cerl ; erl -sname car1@car1 -run normal_state_test normal3_test_
normal3_test_() ->
    % Arrange 

    test_fixture:register(),
    State = test_fixture:default_state3(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync3(State),

    % Act
    receive
        {car_call, Req1} ->
            {Label1, Sender1, _Target1, _RTT1, _Body1} = Req1,
            case Label1 of 
                % launch normal defaultBehaviour
                next ->
                    utils:log("Test: receive next call"),
                    car:default_behaviour(Sender1)
                    %assert(Result1, normal_default_behaviour)
            end
    end,
    receive
        {car_call, Req2} ->
            {Label2, _Sender2, _Target2, _RTT2, _Body2} = Req2,
            case Label2 of 
                % launch timer
                wait ->
                    utils:log("Test: receive wait call"),
                    flow:launch_event(timer, [Req2])
            end
    end,
    receive
        {car_call, Req3} ->
            {Label3, Sender3, _Target3, _RTT3, _Body3} = Req3,
            case Label3 of 
                % receive turn timeout
                wait_reply ->
                    utils:log("Test: receive wait_reply call"),
                    car:default_behaviour(Sender3)
            end
    end.

    %Assert


%cerl ; erl -sname car1@car1 -run normal_state_test normal_crash_test_
normal_crash_test_() ->

    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State#car_state{ delta = 0 }),
    test_fixture:skip_sync(State),

    receive
        {car_call, Req1} ->
            {Label1, Sender1, _Target1, _RTT, _Body1} = Req1,
            case Label1 of 
                % launch normal defaultBehaviour
                next ->
                    utils:log("Test: receive next call"),
                    car:default_behaviour(Sender1)
                    %assert(Result1, normal_default_behaviour)
            end
    end,
    utils:log("Test: launch crash"),
    flow:killer(State#car_state.name, 0),
    receive
        {car_call, Req} ->
            utils:log("Test: killer crash event"),
            {Label, Sender, Target, RTT, Body} = Req,
            utils:log("Test: launch crash on car"),
            car:crash(Sender, 2)
    end.
    %[ ?_assert(Response1 =:= ExpectedResponse1) ].

