%%%===================================================================
%%% Test for the leader state
%%%===================================================================


-module(leader_state_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("car.hrl"). 


% Launch every test with
% erl -sname car1@car1 -run leader_state_test test
  

%erl -sname car1@car1 -run leader_state_test leader_test_
% if there is nobody in front car adj then the car simply change state in normal
leader_test_() ->
    % Arrange
    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync(State),
    test_fixture:skip_normal(State),

    receive
        {car_call, Req1} ->
            {Label1, Sender1, _Target1, _Body1} = Req1,
            case Label1 of 
                % launch normal defaultBehaviour
                next ->
                    utils:log("Supervisor receive next call"),
                    car:default_behaviour(Sender1)
                    %assert(Result1, normal_default_behaviour)
            end            
    end,
    
    % Act 
    car:stop(State#car_state.name).
%        
%    % Assert
%    [?_assert(true =:= true) ].
%
%
%%erl -sname car1@car1 -run leader_state_test leader_test2_
leader_test2_() ->
    % Arrange
    test_fixture:register(),
    State = test_fixture:default_state3(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync3(State),
    test_fixture:skip_normal3(State),
    receive
        {car_call, Req1} ->
            {Label1, Sender1, _Target1, _Body1} = Req1,
            case Label1 of 
                % launch leader defaultBehaviour
                next ->
                    utils:log("Supervisor receive next call"),
                    car:default_behaviour(Sender1)
                    %assert(Result1, normal_default_behaviour)
            end            
    end,
    receive
        {car_call, Req2} ->
            {Label2, _Sender2, _Target2, _Body2} = Req2,
            case Label2 of 
                check ->
                    utils:log("Supervisor receive check call"),
                    utils:log("Car2 receives the check"),
                    %check reponse = risposta di car2 a car1 del check
                    {_Result2, _Data2} = car:check_response({check_response, car2, car1, utils:get_timestamp(), 0, 
                                                           #car_state{  name = car2, 
                                                                        side = 1,
                                                                        crossing = false,
                                                                        position = 1,
                                                                        arrival_time = State#car_state.arrival_time +10
                                                                        }})                            
                                                        
            end
    end,
    receive
        {car_call, Req3} ->
            {Label3, Sender3, _Target3, _Body3} = Req3,
            case Label3 of 
                next ->
                    utils:log("Supervisor receive next call"),
                    car:default_behaviour(Sender3)
            end
    end,
    receive
        {car_call, Req4} ->
            {Label4, Sender4, _Target4, _Body4} = Req4,
            case Label4 of 
                next ->
                    utils:log("Supervisor receive next call"),
                    car:default_behaviour(Sender4)
            end
    end,

    % Act 
    car:stop(State#car_state.name).
    
%    % Assert
%    [?_assert(true =:= true) ].
%
%
%%erl -sname car1@car1 -run leader_state_test leader_test3_
%leader_test3_() ->
%    % Arrange
%    test_fixture:register(),
%    State = test_fixture:default_state3(),
%    car:start_link(State#car_state.name, State),
%    test_fixture:skip_sync3(State),
%    test_fixture:skip_normal3(State),
%
%    % Act 
%    car:stop(State#car_state.name),
%
%    % Assert
%    [?_assert(true =:= true) ].