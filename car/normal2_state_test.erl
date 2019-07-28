%%%===================================================================
%%% Test for the normal state
%%%===================================================================


-module(normal2_state_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("car.hrl"). 


% Launch every test with
% cerl ; erl -sname car1 -setcookie ds-project -run normal2_state_test test

  
% cerl ; erl -sname car1 -setcookie ds-project -run normal2_state_test normal_test_
normal_test_() ->
    % Arrange 

    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync(State),
    test_fixture:skip_normal(State),
    test_fixture:skip_leader(State),

    % Act and Assert
    test_fixture:skip_next(),
    car:stop(State#car_state.name).


% cerl ; erl -sname car1 -setcookie ds-project -run normal2_state_test normal2_test_
normal2_test_() ->

    test_fixture:register(),
    State = test_fixture:default_state2(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync2(State),
    test_fixture:skip_normal2(State),
    test_fixture:skip_leader2(State),

    % Act and Assert
    test_fixture:skip_next(),
    car:stop(State#car_state.name).


% cerl ; erl -sname car1 -setcookie ds-project -run normal2_state_test normal3_test_
normal3_test_() ->
    % Arrange 

    test_fixture:register(),
    State = test_fixture:default_state3(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync3(State),
    test_fixture:skip_normal3(State),
    test_fixture:skip_leader3(State),
    
    % Act and Assert
    test_fixture:skip_next(),
    car:stop(State#car_state.name).