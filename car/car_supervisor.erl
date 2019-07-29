%% @author Edoardo Lenzi, Talissa Dreossi
%% @copyright GPL-3
%% @version 1.0.0


%%%===================================================================
%%% Car supervisor interface
%%%===================================================================

-module( car_supervisor ).
-compile( export_all ).
-include( "car.hrl" ).


start( Args ) -> 
    utils:log( "Supervisor: Args: ~p", [ Args ] ),
    [ WSHost, WSPort, Host, Ip, PSide, PPower, PSize, PBridgeCapacity, PBridgeLength, 
      PMaxSpeed, PTowTruckTime, PMaxRTT, PCrashType, PTimeout ] = Args,
    { Side, _ } = string:to_integer( PSide ),
    { Power, _ } = string:to_integer( PPower ),
    { Size, _ } = string:to_integer( PSize ),
    { BridgeCapacity, _ } = string:to_integer( PBridgeCapacity ),
    { BridgeLength, _ } = string:to_integer( PBridgeLength ),
    { MaxSpeed, _ } = string:to_integer( PMaxSpeed ),
    { TowTruckTime, _ } = string:to_integer( PTowTruckTime ),
    { MaxRTT, _ } = string:to_integer( PMaxRTT ),
    { CrashType, _} = string:to_integer( PCrashType ),
    { Timeout, _ } = string:to_integer( PTimeout ),
    
    register( supervisor, self() ),    

    State = #car_state{
                        name = node(), 
                        side = Side - 1, 
                        power = Power, 
                        size = Size,
                        speed = 0,
                        obstacle_position = 0, 
                        position = ( ( Size / 2 ) + 1) * Side - 1, 
                        last_position = Side - 1,
                        last_crossing = false,
                        crossing = false,
                        synchronized = false,
                        crash_type = 0,
                        delta = 0,
                        adj = #adj{
                                    front_cars = http_client:get_sync( 
                                                                      node(), 
                                                                      Side - 1, 
                                                                      Power, 
                                                                      WSHost, 
                                                                      WSPort
                                                                    ), 
                                    rear_cars = [ ]
                                   }, 
                        state = sync,
                        host = list_to_atom( Host ),
                        ip = list_to_atom( Ip ),
                        ws_host = WSHost,
                        ws_port = WSPort,
                        bridge_capacity = BridgeCapacity, 
                        bridge_length = BridgeLength,
                        max_speed = MaxSpeed,
                        tow_truck_time = TowTruckTime,
                        max_RTT = MaxRTT,
                        last_RTT = 0
                    },
    utils:log( "Supervisor: Car adj ~p", [ State#car_state.adj ] ),
    utils:log( "Supervisor: Timeout: ~p", [ Timeout ] ),
	if Timeout > 0 ->
        utils:log( "Supervisor:Launch killer process with timeout" ),
        flow:launch_event( killer, [ State#car_state.name, State, CrashType, Timeout ] );
    true ->
        ok
    end,
    car:start_link( State#car_state.name, State ),
    car:default_behaviour( State#car_state.name ),
    loop().


loop() ->
    receive

        smoke_test -> 
            utils:log( "Supervisor: receive smoke test" );

        { car_call, Req } ->
            utils:log( "Supervisor: receive car_call ~p ", [ Req ] ),
            { ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody } = Req,
            CurrentTime = utils:get_timestamp(),
            case ReqLabel of 

                adj ->
                    Adj = http_client:get_adj( ReqBody ),
                    car:adj_reply( ReqSender, Adj );

                last_adj ->
                    Last = http_client:get_last_adj( ReqBody ),
                    car:last_adj_reply( ReqSender, Last );

                log_state -> 
                    http_client:get_adj( ReqBody );

                next ->
                    http_client:get_adj( ReqBody ),
                    car:default_behaviour( ReqSender );

                wait ->
                    flow:launch_event( timer, [ Req ] );

                wait_reply ->
                    
                    case ReqBody of 
                        default_behaviour ->
                            car:default_behaviour( ReqSender );

                        tow_truck ->
                            car:tow_truck( ReqSender )
                    end;

                tow_truck_request ->
                    utils:log( "Supervisor: tow truck called" ),
                    flow:launch_event( tow_truck_request, [ { 
                                                              ReqLabel, 
                                                              ReqSender, 
                                                              ReqTarget, 
                                                              CurrentTime, 
                                                              ReqRTT, 
                                                              ReqBody 
                                                           } ] );

                stop -> 
                    http_client:get_adj( ReqBody ),
                    car:stop(ReqTarget),
                    utils:log( "Supervisor: Stop myself" ),
                    init:stop();
                crash ->
                    car:crash( ReqSender, ReqBody );
                % wild-card used for: check, one_way_check, crossing, update_rear, update_front, etc.
                _ ->
                    flow:launch_event( request_timer, [ { 
                                                            ReqLabel, 
                                                            ReqSender, 
                                                            ReqTarget, 
                                                            CurrentTime, 
                                                            ReqRTT, 
                                                            ReqBody
                                                      } ] )
            end;

        { timer_call, Req } ->
            utils:log( "Supervisor: receive timer_call ~p ", [ Req ] ),
            { ReqLabel, ReqSender, ReqTarget, ReqNickname, ReqSendingTime, ReqBody } = Req,
            case ReqLabel of 

                check ->
                    { Result, Data } = car:check( ReqTarget, ReqBody ),
                    
                    if Result =/= dead_ignore -> 
                        supervisor_reply_supervisor_api:sup_reply( { 
                                                                     check_reply, 
                                                                     ReqTarget, 
                                                                     ReqSender, 
                                                                     ReqNickname, 
                                                                     ReqSendingTime, 
                                                                     Data
                                                                  } );
                    true ->
                        ignore
                    end;

                one_way_check ->
                    { Result, _Data } = car:check( ReqTarget, ReqBody ),
                    if Result =/= dead_ignore -> 
                        supervisor_reply_supervisor_api:sup_reply( { 
                                                                     one_way_check_reply, 
                                                                     ReqTarget, 
                                                                     ReqSender, 
                                                                     ReqNickname, 
                                                                     ReqSendingTime, 
                                                                     Result
                                                                  } );
                    true ->
                        ignore
                    end;

                crossing ->
                    { Result, _Data } = car:crossing( Req ),
                    if Result =/= dead_ignore -> 
                        supervisor_reply_supervisor_api:sup_reply( { 
                                                                     crossing_reply, 
                                                                     ReqTarget, 
                                                                     ReqSender, 
                                                                     ReqNickname, 
                                                                     ReqSendingTime, 
                                                                     Result
                                                                 } );
                    true ->
                        ignore
                    end;

                update_rear ->
                    { Result, _Data } = car:update_rear( ReqTarget, ReqBody ),
                    if Result =/= dead_ignore -> 
                        supervisor_reply_supervisor_api:sup_reply( { 
                                                                     update_rear_reply, 
                                                                     ReqTarget, 
                                                                     ReqSender, 
                                                                     ReqNickname, 
                                                                     ReqSendingTime, 
                                                                     Result
                                                                 } );
                    true ->
                        ignore
                    end;

                update_front ->
                    { Result, _Data } = car:update_front( ReqTarget, ReqBody ),
                    if Result =/= dead_ignore -> 
                        supervisor_reply_supervisor_api:sup_reply( { 
                                                                     update_front_reply, 
                                                                     ReqTarget, 
                                                                     ReqSender, 
                                                                     ReqNickname, 
                                                                     ReqSendingTime,
                                                                     Result
                                                                 } );
                    true ->
                        ignore
                    end;

                tow_truck_request -> 
                    { Result, _Data } = car:tow_truck( ReqTarget ),
                    if Result =/= dead_ignore -> 
                        supervisor_reply_supervisor_api:sup_reply( { 
                                                                     tow_truck_reply, 
                                                                     ReqTarget, 
                                                                     ReqSender, 
                                                                     ReqNickname, 
                                                                     ReqSendingTime, 
                                                                     Result
                                                                 } );
                    true ->
                        ignore
                    end
            end;

        { timer_reply, Reply } ->
            utils:log( "Supervisor: receive timer_reply ~p ", [ Reply ] ),
            { ReplyLabel, ReplySender, ReplyTarget, ReplySendingTime, ReplyBody } = Reply,
            RTT = utils:get_timestamp() - ReplySendingTime,
            case ReplyLabel of 
                
                check_reply ->
                    car:check_reply( { ReplySender, ReplyTarget, ReplySendingTime, RTT, ReplyBody } );
                timeout ->
                    car:timeout( ReplyTarget, ReplySender );
                update_adj ->
                    car:adj_reply( ReplyTarget, ReplyBody )
            end;   
        Any ->
            utils:log( "Test: receive unhandled call ~p ", [ Any ] )
    end,
    loop().
