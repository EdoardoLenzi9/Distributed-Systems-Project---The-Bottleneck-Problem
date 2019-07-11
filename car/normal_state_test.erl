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
    car:stop(State#car_state.name).
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
                                                           #car_state{  name = car1, 
                                                                        side = State#car_state.side,
                                                                        speed = 0,
                                                                        size = 1,
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
                                                           #car_state{  name = car1, 
                                                                        side = State#car_state.side,
                                                                        speed = State#car_state.max_speed,
                                                                        size = 1,
                                                                        crossing = false,
                                                                        % moves to position 0
                                                                        position = 0,
                                                                        current_time = utils:get_timestamp()}})
            end
    end,
    receive
        {car_call, Req4} ->
            {Label4, _Sender4, _Target4, _RTT4, _Body4} = Req4,
            case Label4 of 
                check ->
                    utils:log("Test: receive check call"),
                    utils:log("Test: Car2 moves to crossing position -1"),
                    {_Result4, _Data4} = car:check_reply({car2, car1, utils:get_timestamp(), 0, 
                                                           #car_state{  name = car1, 
                                                                        side = State#car_state.side,
                                                                        speed = State#car_state.max_speed,
                                                                        size = 1,
                                                                        crossing = true,
                                                                        % moves to position 0
                                                                        position = -1,
                                                                        current_time = utils:get_timestamp()}})
            end
    end,
    receive
        {car_call, Req5} ->
            {Label5, Sender5, _Target5, _RTT5, _Body5} = Req5,
            case Label5 of 
                check ->
                    utils:log("Test: receive check call"),
                    utils:log("Test: Car2 moves to crossing position 0"),
                    {_Result5, _Data5} = car:check_reply({car2, car1, utils:get_timestamp(), 0, 
                                                           #car_state{  name = car1, 
                                                                        side = State#car_state.side,
                                                                        speed = State#car_state.max_speed,
                                                                        size = 1,
                                                                        crossing = true,
                                                                        % moves to position 0
                                                                        position = 0,
                                                                        current_time = utils:get_timestamp()}}),
                utils:log("Test: send update"),
                car:update(Sender5, [])
            end
    end,
    receive
        {car_call, Req6} ->
            {Label6, Sender6, _Target6, _RTT6, _Body6} = Req6,
            case Label6 of 
                check ->
                    utils:log("Test: receive fail check");
                default_behaviour ->
                    utils:log("Test: receive default behaviour"),
                    car:default_behaviour(Sender6)
            end
    end,
    receive
        {car_call, Req7} ->
            {Label7, Sender7, _Target7, _RTT7, _Body7} = Req7,
            case Label7 of 
                check ->
                    utils:log("Test: receive fail check");
                default_behaviour ->
                    utils:log("Test: receive default behaviour"),
                    car:default_behaviour(Sender7)
            end
    end,
    receive
        {car_call, Req8} ->
            {Label8, _Sender8, _Target8, _RTT8, _Body8} = Req8,
            case Label8 of 
                wait ->
                    utils:log("Test: receive wait call"),
                    flow:launch_event(timer, [Req8])
            end
    end,
    receive
        {car_call, Req9} ->
            {Label9, Sender9, _Target9, _RTT9, _Body9} = Req9,
            case Label9 of 
                % receive turn timeout
                wait_reply ->
                    utils:log("Test: receive wait_reply call"),
                    car:default_behaviour(Sender9)
            end
    end,
    car:stop(State#car_state.name).


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
    car:stop(State#car_state.name).
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
        {car_call, Req2} ->
            utils:log("Test: killer crash event"),
            {_Label2, Sender2, _Target2, _RTT2, _Body2} = Req2,
            utils:log("Test: launch crash on car"),
            car:crash(Sender2, 2)
    end.
    %[ ?_assert(Response1 =:= ExpectedResponse1) ].

