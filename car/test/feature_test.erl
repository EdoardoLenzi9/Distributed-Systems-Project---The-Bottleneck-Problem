%% @author Edoardo Lenzi, Talissa Dreossi
%% @copyright GPL-3
%% @version 1.0.0


%%%===================================================================
%%% Test for generic features
%%%===================================================================


-module( feature_test ).
-compile( export_all ).
-include_lib( "eunit/include/eunit.hrl" ).
-include( "car.hrl" ). 


% Launch every test with
% erl -sname car1 -run feature_test test

% cerl ; erl -sname car1 -run feature_test request_timer_test_
request_timer_test_() ->
    % Arrange
    
    test_fixture:register(),
   
    Request = { check, node(), node(), 1000, {} },
    { Label, Sender, Target, RTT, Body } = Request,
    CurrentTime = utils:get_timestamp(),
    utils:log( "Test: send event to timer" ),
    flow:launch_event( request_timer, [ { Label, Sender, Target, CurrentTime, RTT, Body } ] ),

    receive
        { timer_call, Reply } ->
            utils:log( "Test: receive timer call" ),
            { check, Sender, Target, Nickname, SendingTime, _Body } = Reply,
            utils:log( "Test: send reply to timer" ),
            supervisor_reply_supervisor_api:sup_reply( { check_reply, Target, Sender, Nickname, SendingTime, test_fixture:default_state() } )
    end,
    receive
        { timer_reply, _Reply } ->
            utils:log( "Test: receive timer reply" )
    end.


% cerl ; erl -sname car1 -run feature_test request_timer2_test_
request_timer2_test_() ->
    % Arrange
    
    test_fixture:register(),
   
    Request = { check, node(), node(), 1000, {} },
    { Label, Sender, Target, RTT, Body } = Request,
    CurrentTime = utils:get_timestamp(),
    utils:log( "Test: send event to timer" ),
    flow:launch_event( request_timer, [ { Label, Sender, Target, CurrentTime, RTT, Body } ] ),

    receive
        { timer_call, Reply } ->
            utils:log( "Test: receive timer call and wait until timeout" )
    end,
    receive
        { timer_reply, _Reply } ->
            utils:log( "Test: receive timeout reply" )
    end.