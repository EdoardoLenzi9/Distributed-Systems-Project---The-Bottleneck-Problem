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

    utils:log("Test: there isn't any car in front"),
    utils:log("Test: call defaultBehaviour"),
    {Result1, Data1} = car:default_behaviour(State#car_state.name),

    receive
        {car_call, Req} ->
            {Label, Sender, Target, Body} = Req,
            test_fixture:assert(Body, State),
            case Label of 
                adj ->
                    utils:log("Test: receive adj call, reply with {[], []}"),
                    Adj = #adj{front_cars = [], rear_cars = []},
                    {Result2, Data2} = car:adj_reply(Sender, Adj),
                    ExpectedData2 = State#car_state{    speed = 0, 
                                                        position = State#car_state.side, 
                                                        current_time = Data2#car_state.current_time, 
                                                        arrival_time = Data2#car_state.arrival_time, 
                                                        delta = 0, 
                                                        synchronized = true,
                                                        adj = Adj },
                    test_fixture:assert(Result2, normal),
                    test_fixture:assert(Data2, ExpectedData2)
            end
    end,

    car:stop(State#car_state.name),

    % Assert
    test_fixture:assert(Result1, sync_default_behaviour),
    test_fixture:assert(Data1, State),
    [ ?_assert(Result1 =:= sync_default_behaviour),
      ?_assert(Data1 =:= State) ].


% cerl ; erl -sname car1@car1 -run init_state_test sync2_test_
sync2_test_() ->
    % Arrange

    test_fixture:register(),
    State = test_fixture:default_state2(),
    car:start_link(State#car_state.name, State),
    utils:log("Test: there is another car called car2 in front of car1"),

    % Act and Assert

    utils:log("Test: car1 remains in the default state waiting for a check from car2"),
    {Result1, _Data1} = car:default_behaviour(State#car_state.name),

    receive
        {car_call, Req2} ->
            utils:log("Test: car1 calls its supervisor in order to propagate the check"),
            {Label, Sender, Target, RTT, Body} = Req2,

            %ExpectedRequest2 = {check, car1, car1, 1000, {}},
            %test_fixture:assert(Request2, ExpectedRequest2),
            
            utils:log("Test: supervisor starts a request_timer"),
            flow:launch_event(request_timer, [{Label, Sender, Target, utils:get_timestamp(), RTT, Body}])
    end,
    receive
        {timer_call, Req3} ->
            utils:log("Test: timer call car2 supervisor"),
            {Label3, Sender3, Target3, Nickname3, SendingTime3, Body3} = Req3,

            %ExpectedRequest3 = {check, car1, car1, 1000, {}},
            %test_fixture:assert(Request2, ExpectedRequest2),

            utils:log("Test: car2 supervisor calls check"),
            {Result4, Data4} = car:check(Req3),
            supervisor_reply_supervisor_api:sup_reply({check_reply, Target3, Sender3, Nickname3, SendingTime3, Data4})
    end,
    receive
        {timer_reply, Reply5} ->
            utils:log("Test: receive timer reply"),
            {Label5, Sender5, Target5, SendingTime5, Body5} = Reply5,
            RTT5 = utils:get_timestamp() - SendingTime5,
            utils:log("Test: send check reply"),
            {Result6, Data6} = car:check_reply({Sender5, Target5, SendingTime5, RTT5, Body5})
    end,
    receive
        {car_call, Req6} ->
            {Label6, Sender6, Target6, RTT6, Body6} = Req6,
            case Label6 of 
                adj ->
                    utils:log("Test: receive adj call"),
                    Adj = State#car_state.adj,
                    {Result2, Data2} = car:adj_reply(Sender, Adj)
                    %ExpectedData2 = State#car_state{    speed = 0, 
                    %                                    position = State#car_state.side, 
                    %                                    current_time = Data2#car_state.current_time, 
                    %                                    arrival_time = Data2#car_state.arrival_time, 
                    %                                    delta = 0, 
                    %                                    synchronized = true,
                    %                                    adj = Adj },
                    %test_fixture:assert(Result2, normal),
                    %test_fixture:assert(Data2, ExpectedData2)
            end
    end,
        
    car:stop(State#car_state.name).

    % Assert
    
    %test_fixture:assert(Result1, ExpectedResult1),
    %test_fixture:assert(Result3, ExpectedResult3),
    %test_fixture:assert(Result5, ExpectedResult5),
    %
    %[   ?_assert(Result1 =:= ExpectedResult1),
    %    %?_assert(Result3 =:= ExpectedResult3),
    %    ?_assert(Result5 =:= ExpectedResult5) ].


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
        {car_reply, Reply4} ->
            {Label4, Sender4, Target4, Time4, _Body4} = Reply4,
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
            {Label, Sender, Target, RTT, Body} = Request1,
            flow:launch_event(request_timer, [{Label, Sender, Target, utils:get_timestamp(), RTT, Body}])
    end,
    receive
        {timer_call, Reply} ->
            utils:log("Test: receive timer call and wait until timeout")
    end,
    receive
        {timer_reply, _Reply} ->
            utils:log("Test: receive timeout reply")
    end.


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