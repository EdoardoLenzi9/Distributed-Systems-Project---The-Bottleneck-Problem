%%%===================================================================
%%% Test for the init state
%%%===================================================================


-module(init_state_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("car.hrl"). 


% Launch every test with
% erl -sname car1@car1 -run init_state_test test

% cerl ; erl -sname car1@car1 -run init_state_test sync_test_
sync_test_() ->
    % Arrange
    
    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State),
    
    % Act    

    % if there isn't any car in front, sync transit in the normal state
    utils:log("Supervisor call defaultBehaviour"),
    Result1 = car:default_behaviour(State#car_state.name),
    ExpectedResult1 = {sync_default_behaviour, State#car_state{ delta = 0 }},

    receive
        {car_call, Req} ->
            {Label, Sender, Target, Body} = Req,
            test_fixture:assert(Body, State),
            case Label of 
                adj ->
                    utils:log("Supervisor receive adj call"),
                    % respond with no adj
                    Adj = #adj{front_cars = [], rear_cars = []},
                    {Result2, Data2} = car:adj_reply({Label, Sender, Target, utils:get_timestamp(), 0, Adj}),
                    ExpectedData2 = State#car_state{    speed = 0, 
                                                        position = State#car_state.side, 
                                                        current_time = Data2#car_state.current_time, 
                                                        arrival_time = Data2#car_state.arrival_time, 
                                                        delta = 0, 
                                                        synchronized = true,
                                                        adj = Adj },
                    test_fixture:assert(normal, Result2),
                    test_fixture:assert(ExpectedData2, Data2)
            end
    end,

    car:stop(State#car_state.name),

    % Assert
    
    test_fixture:assert(Result1, ExpectedResult1),
    [ ?_assert(Result1 =:= ExpectedResult1) ].


% cerl ; erl -sname car1@car1 -run init_state_test sync2_test_
sync2_test_() ->
    % Arrange

    test_fixture:register(),
    State = test_fixture:default_state2(),
    % there is another car called car2 in front of car1
    car:start_link(State#car_state.name, State),

    % Act and Assert

    % car1 remains in the default state waiting for a check from car2
    {Result1, _Data1} = car:default_behaviour(State#car_state.name),
    ExpectedResult1 = sync_default_behaviour,

    % car1 calls its supervisor in order to propagate the check
    receive
        {car_call, Request2} ->
            ExpectedRequest2 = {check, car1, car2, {}},
            test_fixture:assert(Request2, ExpectedRequest2)
    end,

    TimeStamp3 = utils:get_timestamp(),
    RTT = utils:get_timestamp() - TimeStamp3,
    {Result5, _Delta} = car:check_reply({check, car2, car1, TimeStamp3, RTT, State#car_state{current_time = utils:get_timestamp()}}),
    ExpectedResult5 = sync_check_reply,
    
    receive
        {car_call, Req} ->
            {Label, Sender, Target, _Body} = Req,
            case Label of 
                adj ->
                    Adj = #adj{front_cars = [#car_state{ name = car2, side = -1, position = 0 }], rear_cars = []},
                    {Result6, Data6} = car:adj_reply({Label, Sender, Target, TimeStamp3, 0, Adj}),
                    ExpectedData6 = State#car_state{speed = 0, position = State#car_state.side, arrival_time = Data6#car_state.arrival_time, current_time = Data6#car_state.current_time, delta = Data6#car_state.delta, adj = Adj, synchronized = true},
                    utils:log("~p", [Data6]),
                    utils:log("~p", [ExpectedData6]),
                    test_fixture:assert(Result6, normal),
                    test_fixture:assert(Data6, ExpectedData6)
            end
    end,
        
    car:stop(State#car_state.name),

    % Assert
    
    test_fixture:assert(Result1, ExpectedResult1),
    %test_fixture:assert(Result3, ExpectedResult3),
    test_fixture:assert(Result5, ExpectedResult5),
    
    [   ?_assert(Result1 =:= ExpectedResult1),
        %?_assert(Result3 =:= ExpectedResult3),
        ?_assert(Result5 =:= ExpectedResult5) ].


% cerl ; erl -sname car1@car1 -run init_state_test sync3_test_
sync3_test_() ->
    % Arrange

    test_fixture:register(),
    State = test_fixture:default_state3(),
    
    % there is another car called car2 in front of car1 on the other side of the bridge
    car:start_link(State#car_state.name, State),

    % Act and Assert

    % car1 remains in the default state waiting for a check from car2
    {Result1, _Data1} = car:default_behaviour(State#car_state.name),
    ExpectedResult1 = sync_default_behaviour,

    % car1 calls its supervisor in order to propagate the check
    receive
        {car_call, Request2} ->
            ExpectedRequest2 = {check, car1, car2, {}},
            test_fixture:assert(Request2, ExpectedRequest2)
    end,

    % send a check event to the car
    TimeStamp3 = utils:get_timestamp(),
    Request3 = {check, car1, car1, TimeStamp3, {}},
    {Result3, _Data} = car:check(Request3),
    ExpectedResult3 = sync_check,

    % receive car reply with current state
    receive
        {car_reply, Response4} ->
            {Label4, Sender4, Target4, Time4, _Body4} = Response4,
            test_fixture:assert({Label4, Sender4, Target4, Time4}, {check_reply, car1, car1, TimeStamp3})
    end,

    % send check reply to the car
    RTT = utils:get_timestamp() - TimeStamp3,
    {Result5, _Delta} = car:check_reply({check, car2, car1, TimeStamp3, RTT, State#car_state{current_time = utils:get_timestamp()}}),
    ExpectedResult5 = sync_check_reply,

    receive
        {car_call, Req} ->
            {Label, Sender, Target, _Body} = Req,
            case Label of 
                adj ->
                    Adj = #adj{front_cars = [#car_state{ name = car2, side = -1, position = 0 }], rear_cars = []},
                    {Result6, Data6} = car:adj_reply({Label, Sender, Target, TimeStamp3, 0, Adj}),
                    ExpectedData6 = State#car_state{speed = 0, position = State#car_state.side, arrival_time = Data6#car_state.arrival_time, current_time = Data6#car_state.current_time, delta = Data6#car_state.delta, adj = Adj, synchronized = true},
                    utils:log("~p", [Data6]),
                    utils:log("~p", [ExpectedData6]),
                    test_fixture:assert(Result6, normal),
                    test_fixture:assert(Data6, ExpectedData6)
            end
    end,
        
    car:stop(State#car_state.name),

    % Assert

    test_fixture:assert(Result1, ExpectedResult1),
    test_fixture:assert(Result3, ExpectedResult3),
    test_fixture:assert(Result5, ExpectedResult5),

    [   ?_assert(Result1 =:= ExpectedResult1),
        ?_assert(Result3 =:= ExpectedResult3),
        ?_assert(Result5 =:= ExpectedResult5) ].


% cerl ; erl -sname car1@car1 -run init_state_test sync_check_timeout_test_
sync_check_timeout_test_() ->
    test_fixture:register(),
    State = test_fixture:default_state2(),
    car:start_link(State#car_state.name, State),
    car:default_behaviour(State#car_state.name),

    % Act and Assert
    receive
        {car_call, Request1} ->
            ExpectedRequest2 = {check, car1, car2, {}}
            %test_fixture:assert(Request2, ExpectedRequest2)
    end,

    TimeStamp3 = utils:get_timestamp(),
    RTT = utils:get_timestamp() - TimeStamp3,
    {Result5, _Delta} = car:check_reply({check, car2, car1, TimeStamp3, RTT, State#car_state{current_time = utils:get_timestamp()}}),
    ExpectedResult5 = sync_check_reply,

    receive
        {car_call, Req} ->
            {Label, Sender, Target, _Body} = Req,
            case Label of 
                adj ->
                    Adj = #adj{front_cars = [#car_state{ name = car2, side = -1, position = 0 }], rear_cars = []},
                    {Result6, Data6} = car:adj_reply({Label, Sender, Target, TimeStamp3, 0, Adj}),
                    ExpectedData6 = State#car_state{speed = 0, position = State#car_state.side, arrival_time = Data6#car_state.arrival_time, current_time = Data6#car_state.current_time, delta = Data6#car_state.delta, adj = Adj, synchronized = true},
                    utils:log("~p", [Data6]),
                    utils:log("~p", [ExpectedData6]),
                    test_fixture:assert(Result6, normal),
                    test_fixture:assert(Data6, ExpectedData6)
            end
    end,
        
    car:stop(State#car_state.name),

    % Assert

    %test_fixture:assert(Result1, ExpectedResult1),
    %test_fixture:assert(Result3, ExpectedResult3),
    test_fixture:assert(Result5, ExpectedResult5),

    [   %?_assert(Result1 =:= ExpectedResult1),
        %?_assert(Result3 =:= ExpectedResult3),
        ?_assert(Result5 =:= ExpectedResult5) ].



% erl -sname car1@car1 -run init_state_test sync_crash_test_
sync_crash_test_() ->
    % Arrange

    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State),
    car:default_behaviour(State#car_state.name),
    % Act and Assert

    utils:log("before crash"),
    flow:killer(State#car_state.name, 0),
    utils:log("after crash"),
        
    car:stop(State#car_state.name).


% erl -sname car1@car1 -run init_state_test sync_crash2_test_
sync_crash2_test_() ->
    % Arrange

    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State),
    car:default_behaviour(State#car_state.name),
    % Act and Assert

    utils:log("before crash"),
    flow:killer(State#car_state.name, 1000),
    utils:log("after crash"),
    timer:apply_after(1000, car, stop, [State#car_state.name]).