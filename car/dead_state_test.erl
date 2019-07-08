%%%===================================================================
%%% Test for the leader state
%%%===================================================================


-module(dead_state_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("car.hrl"). 


% Launch every test with
% erl -sname car1@car1 -run crash_state_test test
  

%erl -sname car1@car1 -run leader_state_test leader_test_
% if there is nobody in front car adj then the car simply change state in normal
dead_test_() ->
    % Arrange
    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync(State),
    
    %must set timeout 

    receive
        {car_call, Req1} ->
            {Label1, Sender1, _Target1, _Body1} = Req1,
            case Label1 of 
                % launch normal defaultBehaviour
                crash ->
                    utils:log("crash"),
                    car:default_behaviour(Sender1)
                    %assert(Result1, normal_default_behaviour)
            end            
    end,
    
    % Act 
    car:stop(State#car_state.name).