%% @author Edoardo Lenzi, Talissa Dreossi
%% @copyright GPL-3
%% @version 1.0.0


%%%===================================================================
%%% Test for the init state
%%%===================================================================


-module( init_state_test ).
-compile( export_all ).
-include_lib( "eunit/include/eunit.hrl" ).
-include( "car.hrl" ). 


% Launch every test with
% cerl ; erl -sname car1 -setcookie ds-project -run init_state_test test

% cerl ; erl -sname car1 -setcookie ds-project -run init_state_test sync_test_
sync_test_() ->
    % Arrange
    
    test_fixture:register(),
    State = test_fixture:default_state(),
    car:start_link( State#car_state.name, State ),
    
    % Act    

    utils:log( "Test: there isn't any car in front" ),
    utils:log( "Test: call defaultBehaviour" ),
    car:default_behaviour( State#car_state.name ),

    test_fixture:listen( adj, fun( _, Sender, _, _, _ ) -> 
        Adj = #adj{ front_cars = [], rear_cars = [] },
        { _Result, _Data } = car:adj_reply( Sender, Adj )
    end),

    car:stop( State#car_state.name ).


% cerl ; erl -sname car1 -setcookie ds-project -run init_state_test sync2_test_
sync2_test_() ->
    sync_common_test( test_fixture:default_state2() ).


% cerl ; erl -sname car1 -setcookie ds-project -run init_state_test sync3_test_
sync3_test_() ->
    sync_common_test( test_fixture:default_state3() ).


sync_common_test( State ) ->
    % Arrange

    test_fixture:register(),
    car:start_link( State#car_state.name, State ),
    utils:log( "Test: there is another car called car2 in front of car1" ),

    % Act and Assert

    utils:log( "Test: car1 remains in the default state waiting for a check from car2" ),
    car:default_behaviour( State#car_state.name ),

    test_fixture:listen( check, fun( ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody ) -> 
        flow:launch_event( request_timer, [ { ReqLabel, ReqSender, ReqTarget, utils:get_timestamp(), ReqRTT, ReqBody } ] )   
    end ),
    test_fixture:listen( check, fun( _ReqLabel, ReqSender, ReqTarget, ReqNickname, ReqSendingTime, _ReqBody ) -> 
        { _Result, Data } = car:check( ReqTarget ),
        supervisor_reply_supervisor_api:sup_reply( { check_reply, ReqTarget, ReqSender, ReqNickname, ReqSendingTime, Data } )
    end ),
    test_fixture:listen( check_reply, fun( _ReplyLabel, ReplySender, ReplyTarget, ReplySendingTime, ReplyBody ) -> 
        RTT = utils:get_timestamp() - ReplySendingTime,
        { _Result, _Data } = car:check_reply( { ReplySender, ReplyTarget, ReplySendingTime, RTT, ReplyBody } )
    end),
    test_fixture:listen( adj, fun( _ReplyLabel, ReplySender, _ReplyTarget, _ReplySendingTime, _ReplyBody ) -> 
        Adj = State#car_state.adj,
        { _Result, _Data } = car:adj_reply( ReplySender, Adj )
    end),
        
    car:stop( State#car_state.name ).


% cerl ; erl -sname car1 -setcookie ds-project -run init_state_test sync_check_timeout_test_
sync_check_timeout_test_() ->
    % Arrange

    test_fixture:register(),
    State = test_fixture:default_state2(),
    utils:log( "Test: there is another car called car2 in front of car1" ),
    car:start_link( State#car_state.name, State ),
    utils:log( "Test: car1 remains in the default state waiting for a check from car2" ),
    car:default_behaviour( State#car_state.name ),

    % Act and Assert

    test_fixture:listen( check, fun( ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody ) -> 
        flow:launch_event( request_timer, [ { ReqLabel, ReqSender, ReqTarget, utils:get_timestamp(), ReqRTT, ReqBody } ] )
    end ),
    test_fixture:listen( check, fun( _ReqLabel, _ReqSender, _ReqTarget, _ReqNickname, _ReqSendingTime, _ReqBody ) -> 
        utils:log( "wait until timeout" )   
    end ),
    test_fixture:listen( timeout, fun( _ReplyLabel, ReplySender, ReplyTarget, _ReplySendingTime, _ReplyBody ) -> 
        car:timeout( ReplySender, ReplyTarget )
    end ),
    test_fixture:listen( tow_truck_request, fun( _ReqLabel, ReqSender, ReqTarget, _ReqRTT, ReqBody ) -> 
        flow:launch_event( tow_truck, [ReqBody, ReqTarget ] ),
        utils:log( "Test: call car crash (will be postponed)" ),
        car:crash( ReqTarget, 2 ),
        car:update_front( ReqSender, [] ),
        car:default_behaviour( ReqSender )
    end ),
    test_fixture:listen( default_behaviour, fun( _ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody ) -> 
        car:default_behaviour( ReqSender )
    end ),
    test_fixture:listen( adj, fun( _ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody ) -> 
        Adj = State#car_state.adj,
        car:adj_reply( ReqSender, Adj )
    end ),
    test_fixture:listen( adj, fun( _ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody ) -> 
        ok
    end ),
    test_fixture:listen( next, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody ) -> 
        car:default_behaviour( ReqSender )
    end ),
    test_fixture:listen( crash, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody ) -> 
        car:crash( ReqSender, 2 )
    end ),
    test_fixture:listen( check, fun(_ReqLabel, _ReqSender, _ReqTarget, _ReqRTT, _ReqBody ) -> 
        ok
    end),
    test_fixture:skip_log_state(),
    test_fixture:listen( next, fun( _ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody ) -> 
        car:default_behaviour( ReqSender )
    end ),
    test_fixture:listen( tow_truck, fun( _ReqLabel, _ReqSender, ReqTarget, _ReqRTT, _ReqBody ) -> 
        car:tow_truck( ReqTarget )
    end ),
    % TODO
    test_fixture:listen( update_rear, fun( _ReqLabel, _ReqSender, _ReqTarget, _ReqRTT, _ReqBody ) -> 
        ok
    end ),
    test_fixture:listen( stop, fun(_ReqLabel, _ReqSender, ReqTarget, _ReqRTT, _ReqBody ) -> 
        car:stop( ReqTarget )
    end ).