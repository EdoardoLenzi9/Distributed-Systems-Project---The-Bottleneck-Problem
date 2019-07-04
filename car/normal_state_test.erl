%%%===================================================================
%%% Test for the normal state
%%%===================================================================


-module(normal_state_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("car.hrl"). 

% erl -sname car1@car1 -run normal_state_test test
% erl -sname car1@car1 -run normal_state_test normal_test_
% normal_state_test:normal_test_().
  

%erl -sname car1@car1 -run normal_state_test normal_test_
normal_test_() ->

    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State#car_state{ delta = 0 }),
    test_fixture:skip_sync(State),

    receive
        {car_call, Req1} ->
            {Label1, Sender1, Target1, Body1} = Req1,
            case Label1 of 
                % launch normal defaultBehaviour
                next ->
                    utils:log("Supervisor receive next call"),
                    car:default_behaviour(Sender1)
                    %assert(Result1, normal_default_behaviour)
            end
    end,
    receive
        {car_call, Req2} ->
            {Label2, Sender2, Target2, Body2} = Req2,
            case Label2 of 
                % launch timer
                wait ->
                    utils:log("Supervisor receive wait call"),
                    flow:launch_event(timer, [Req2])
            end
    end,
    receive
        {car_call, Req3} ->
            {Label3, Sender3, Target3, Body3} = Req3,
            case Label3 of 
                % receive turn timeout
                wait_response ->
                    utils:log("Supervisor receive wait_response call"),
                    car:default_behaviour(Sender3)
            end
    end.
    %[ ?_assert(Response1 =:= ExpectedResponse1) ].


%erl -sname car1@car1 -run normal_state_test normal_test_
normal_test1_() ->

    test_fixture:register(),
    State = test_fixture:default_state2(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync2(State),

    receive
        {car_call, Req1} ->
            {Label1, Sender1, Target1, Body1} = Req1,
            case Label1 of 
                % launch normal defaultBehaviour
                next ->
                    utils:log("Supervisor receive next call"),
                    car:default_behaviour(Sender1)
                    %assert(Result1, normal_default_behaviour)
            end
    end,
    receive
        {car_call, Req2} ->
            {Label2, Sender2, Target2, Body2} = Req2,
            case Label2 of 
                % launch timer
                check ->
                    utils:log("Supervisor receive check call"),
                    {Result2, Data2} = car:check_response({response_check, car2, car1, utils:get_timestamp(), 0, 
                                                           #car_state{  name = car2, 
                                                                        side = State#car_state.side,
                                                                        speed = 0,
                                                                        position = 0,
                                                                        current_time = utils:get_timestamp()}})
            end
    end,
    receive
        {car_call, Req3} ->
            {Label3, Sender3, Target3, Body3} = Req3,
            case Label3 of 
                % launch timer
                check ->
                    utils:log("Supervisor receive check call"),
                    {Result2, Data2} = car:check_response({response_check, car2, car1, utils:get_timestamp(), 0, 
                                                           #car_state{  name = car2, 
                                                                        side = State#car_state.side,
                                                                        speed = State#car_state.max_speed,
                                                                        crossing = true,
                                                                        position = 1,
                                                                        current_time = utils:get_timestamp()}})
            end
    end.
    %[ ?_assert(Response1 =:= ExpectedResponse1) ].
