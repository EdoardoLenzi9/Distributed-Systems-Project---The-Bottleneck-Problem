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
            {Label, Sender, Target, RTT, Body} = Req,
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
    sync_common_test(test_fixture:default_state2()).


% cerl ; erl -sname car1@car1 -run init_state_test sync3_test_
sync3_test_() ->
    sync_common_test(test_fixture:default_state3()).


sync_common_test(State) ->
    % Arrange

    test_fixture:register(),
    car:start_link(State#car_state.name, State),
    utils:log("Test: there is another car called car2 in front of car1"),

    % Act and Assert

    utils:log("Test: car1 remains in the default state waiting for a check from car2"),
    {Result1, Data1} = car:default_behaviour(State#car_state.name),

    receive
        {car_call, Req2} ->
            utils:log("Test: car1 calls its supervisor in order to propagate the check"),
            {Label, Sender, Target, RTT, Body} = Req2,

            ExpectedRequest2 = {check, car1, car1, 1000, {}},
            test_fixture:assert(Req2, ExpectedRequest2),
            
            utils:log("Test: supervisor starts a request_timer"),
            flow:launch_event(request_timer, [{Label, Sender, Target, utils:get_timestamp(), RTT, Body}])
    end,
    receive
        {timer_call, Req3} ->
            utils:log("Test: timer call car2 supervisor"),
            {Label3, Sender3, Target3, Nickname3, SendingTime3, Body3} = Req3,

            ExpectedRequest3 = {check, car1, car1, Nickname3, SendingTime3, {}},
            test_fixture:assert(Req3, ExpectedRequest3),

            utils:log("Test: car2 supervisor calls check"),
            {Result4, Data4} = car:check(Req3),

            test_fixture:assert(Result4, sync_check),
            test_fixture:assert(Data4, State#car_state{current_time = Data4#car_state.current_time}),

            supervisor_reply_supervisor_api:sup_reply({check_reply, Target3, Sender3, Nickname3, SendingTime3, Data4})
    end,
    receive
        {timer_reply, Reply5} ->
            utils:log("Test: receive timer reply"),
            {Label5, Sender5, Target5, SendingTime5, Body5} = Reply5,

            ExpectedReply5 = {check_reply, car1, car1, SendingTime5, State#car_state{current_time = Data4#car_state.current_time}},
            test_fixture:assert(Reply5, ExpectedReply5),

            RTT5 = utils:get_timestamp() - SendingTime5,
            utils:log("Test: send check reply"),
            {Result6, Data6} = car:check_reply({Sender5, Target5, SendingTime5, RTT5, Body5}),
            test_fixture:assert(Result6, sync_check_reply),
            test_fixture:assert(Data6, State#car_state{
                current_time = Data6#car_state.current_time,
                arrival_time = Data6#car_state.arrival_time,
                delta =  Data6#car_state.delta})
    end,
    receive
        {car_call, Req6} ->
            {Label6, Sender6, Target6, RTT6, Body6} = Req6,
            case Label6 of 
                adj ->
                    utils:log("Test: receive adj call"),
                    Adj = State#car_state.adj,
                    {Result7, Data7} = car:adj_reply(Sender, Adj),
                    test_fixture:assert(Result7, normal),
                    test_fixture:assert(Data7, State#car_state{
                        position = Data7#car_state.position,
                        current_time = Data7#car_state.current_time,
                        arrival_time = Data7#car_state.arrival_time,
                        delta =  Data7#car_state.delta,
                        synchronized = true})
            end
    end,
        
    car:stop(State#car_state.name),

    % Assert
    
    test_fixture:assert(Result1, sync_default_behaviour),
    test_fixture:assert(Data1, State),
    [ ?_assert(Result1 =:= sync_default_behaviour),
    ?_assert(Data1 =:= State) ].


% cerl ; erl -sname car1@car1 -run init_state_test sync_check_timeout_test_
sync_check_timeout_test_() ->
    test_fixture:register(),
    State = test_fixture:default_state2(),
    car:start_link(State#car_state.name, State),
    utils:log("Test: there is another car called car2 in front of car1"),

    % Act and Assert

    utils:log("Test: car1 remains in the default state waiting for a check from car2"),
    {Result1, Data1} = car:default_behaviour(State#car_state.name),

    receive
        {car_call, Req2} ->
            utils:log("Test: car1 calls its supervisor in order to propagate the check"),
            {Label, Sender, Target, RTT, Body} = Req2,

            ExpectedRequest2 = {check, car1, car1, 1000, {}},
            test_fixture:assert(Req2, ExpectedRequest2),
            
            utils:log("Test: supervisor starts a request_timer"),
            flow:launch_event(request_timer, [{Label, Sender, Target, utils:get_timestamp(), RTT, Body}])
    end,
    receive
        {timer_call, Req3} ->
            utils:log("Test: timer call car2 supervisor"),
            {Label3, Sender3, Target3, Nickname3, SendingTime3, Body3} = Req3,

            ExpectedRequest3 = {check, car1, car1, Nickname3, SendingTime3, {}},
            test_fixture:assert(Req3, ExpectedRequest3)
    end,
    receive
        {timer_reply, Reply5} ->
            utils:log("Test: receive timeout"),
            {Label5, Sender5, Target5, SendingTime5, Body5} = Reply5,
            car:timeout(Sender5, Target5)
    end,
    receive
        {car_call, Req6} ->
            utils:log("Test: receive call_tow_truck"),
            {Label6, Sender6, Target6, RTT6, Body6} = Req6,
            flow:launch_event(tow_truck, [Body6, Target6]),
            utils:log("Test: call car crash (will be postponed)"),
            car:update(Sender6, []),
            car:crash(Target6),
            car:default_behaviour(Sender6)
    end,
    receive
        {car_call, Req7} ->
            {Label7, Sender7, Target7, RTT7, Body7} = Req7,
            case Label7 of 
                adj ->
                    utils:log("Test: receive adj call"),
                    Adj = State#car_state.adj,
                    {Result7, Data7} = car:adj_reply(Sender, Adj)
                    %test_fixture:assert(Result7, normal),
                    %test_fixture:assert(Data7, State#car_state{
                    %    position = Data7#car_state.position,
                    %    current_time = Data7#car_state.current_time,
                    %    arrival_time = Data7#car_state.arrival_time,
                    %    delta =  Data7#car_state.delta,
                    %    synchronized = true})
            end
    end,
    receive
        {car_call, Req7} ->
            utils:log("Test: receive tow_truck"),
            {Label7, Sender7, Target7, RTT7, Body7} = Req7,
            utils:log("Test: launch dead default_behaviour"),
            car:default_behaviour(Target)
    end,
        
    % Assert
    
    test_fixture:assert(Result1, sync_default_behaviour),
    test_fixture:assert(Data1, State),
    [ ?_assert(Result1 =:= sync_default_behaviour),
    ?_assert(Data1 =:= State) ].


% cerl ; erl -sname car1@car1 -run init_state_test sync_crash_test_
sync_crash_test_() ->
    % Arrange

    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State),
    car:default_behaviour(State#car_state.name),
    % Act and Assert

    utils:log("Test: call killer process"),
    flow:launch_event(killer, [State#car_state.name, 0]),
        
    car:stop(State#car_state.name).
