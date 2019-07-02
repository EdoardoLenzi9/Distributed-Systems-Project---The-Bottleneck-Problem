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


default_state() ->
    Env = utils:load_environment(),
    #car_state{
        name = car1, 
        side = -1, 
        power = 2, 
        speed = 0,
        crossing = false,
        adj = #adj{front_cars = [], rear_cars = []}, 
        arrival_time = utils:get_timestamp(), 
        state = init,
        turn = 1000,
        bridge_capacity = 5, 
        bridge_length = 3,
        max_speed = Env#env.max_speed,
        tow_truck_time = Env#env.tow_truck_time,
        max_RTT = Env#env.max_RTT
    }.


%sync_alone_test_() ->
%
%    register(supervisor, self()),    
%       
%    State = default_state(),
%    car:start_link(State#car_state.name, State),
%    
%    % if there isn't any car in front, sync transit in the normal state
%    Result1 = car:default_behaviour(State#car_state.name),
%    ExpectedResult1 = {normal, State},
%
%    car:stop(State#car_state.name),
%    [ ?_assert(Result1 =:= ExpectedResult1) ].


% erl -sname car1@car1 -run init_state_test sync_test_
sync_test_() ->
    case whereis(supervisor) of
        undefined ->
            register(supervisor, self());
        _ ->
            ok
    end,

    State = default_state(),
    % there is another car called car2 in front of car1
    car:start_link(State#car_state.name, State#car_state{ adj = #adj{ front_cars = [ #car_state{ name = car2 } ] }}),

    % car1 remains in the default state waiting for a check from car2
    {Result1, _Data1} = car:default_behaviour(State#car_state.name),
    ExpectedResult1 = sync_default_behaviour,

    % car1 calls its supervisor in order to propagate the check
    receive
        {car_call, Request2} ->
            ExpectedRequest2 = {check, car1, car2, {}},
            assert(Request2, ExpectedRequest2)
    end,

    % send a check event to the car
    TimeStamp3 = utils:get_timestamp(),
    Request3 = {check, car1, car1, TimeStamp3, {}},
    {Result3, _Data} = car:check(Request3),
    ExpectedResult3 = sync_check,


    % receive car response with current state
    receive
        {car_response, Response4} ->
            {Label4, Sender4, Target4, Time4, _Body4} = Response4,
            assert({Label4, Sender4, Target4, Time4}, {check_response, car1, car1, TimeStamp3})
    end,

    % send check response to the car
    RTT = utils:get_timestamp() - TimeStamp3,
    {Result5, _Delta} = car:check_response({check, car2, car1, TimeStamp3, RTT, State#car_state{current_time = utils:get_timestamp()}}),
    ExpectedResult5 = normal,

    assert(Result1, ExpectedResult1),
    assert(Result3, ExpectedResult3),
    assert(Result5, ExpectedResult5),
    
    [   ?_assert(Result1 =:= ExpectedResult1),
        ?_assert(Result3 =:= ExpectedResult3),
        ?_assert(Result5 =:= ExpectedResult5) ].



assert(CurrentResult, ExpectedResult) ->
    if CurrentResult == ExpectedResult ->
        ok;
    true -> 
        throw("test fail")
    end.