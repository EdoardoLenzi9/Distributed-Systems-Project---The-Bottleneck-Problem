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


% erl -sname car1@car1 -run init_state_test sync_alone_test_
sync_alone_test_() ->
    % Arrange
    
    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State),
    
    % Act    

    % if there isn't any car in front, sync transit in the normal state
    Result1 = car:default_behaviour(State#car_state.name),
    ExpectedResult1 = {sync_default_behaviour, State#car_state{ delta = 0}},

    receive
        {car_call, Req} ->
            {Label, Sender, Target, Body} = Req,
            assert(Body, State),
            case Label of 
                adj ->
                    Adj = #adj{front_cars = [], rear_cars = []},
                    {Result2, Data2} = car:adj_response({Label, Sender, Target, utils:get_timestamp(), 0, Adj}),
                    ExpectedData2 = State#car_state{    speed = 0, 
                                                        position = State#car_state.side, 
                                                        current_time = Data2#car_state.current_time, 
                                                        arrival_time = Data2#car_state.arrival_time, 
                                                        delta = 0, 
                                                        adj = Adj },
                    utils:log("~p", [Data2]),
                    utils:log("~p", [ExpectedData2]),
                    assert(ExpectedData2, Data2)
            end
    end,

    car:stop(State#car_state.name),

    % Assert
    
    assert(Result1, ExpectedResult1),
    [ ?_assert(Result1 =:= ExpectedResult1) ].


% erl -sname car1@car1 -run init_state_test sync_test_
sync_test_() ->
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
    ExpectedResult5 = sync_response_check,
    
    receive
        {car_call, Req} ->
            {Label, Sender, Target, _Body} = Req,
            case Label of 
                adj ->
                    Adj = #adj{front_cars = [#car_state{ name = car2, side = -1, position = 0 }], rear_cars = []},
                    car:adj_response({Label, Sender, Target, TimeStamp3, 0, Adj})
                    %{Result6, Data6} = car:adj_response({Label, Sender, Target, TimeStamp3, 0, Adj}),
                    %ExpectedData6 = State#car_state{speed = 0, position = State#car_state.side, arrival_time = Data6#car_state.arrival_time, current_time = Data6#car_state.current_time, delta = Data6#car_state.delta, adj = Adj},
                    %utils:log("~p", [Data6]),
                    %utils:log("~p", [ExpectedData6]),
                    %assert(Result6, normal),
                    %assert(Data6, ExpectedData6)
            end
    end,
        
    car:stop(State#car_state.name),

    % Assert
    
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