%%%===================================================================
%%% Test for the leader state
%%%===================================================================


-module(leader_state_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("car.hrl"). 

% erl -sname car1@car1 -run leader_state_test test
% erl -sname car1@car1 -run leader_state_test leader_test_
% leader_state_test:leader_test_().
  

%erl -sname car1@car1 -run leader_state_test leader_test_
leader_test_() ->
    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync(State),
    test_fixture:skip_normal(State).


%erl -sname car1@car1 -run leader_state_test leader_test2_
leader_test2_() ->
    test_fixture:register(),
    State = test_fixture:default_state2(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync2(State),
    test_fixture:skip_normal2(State).


%erl -sname car1@car1 -run leader_state_test leader_test3_
leader_test3_() ->
    test_fixture:register(),
    State = test_fixture:default_state3(),
    car:start_link(State#car_state.name, State),
    test_fixture:skip_sync3(State),
    test_fixture:skip_normal3(State).