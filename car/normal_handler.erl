-module( normal_handler ).

-compile(export_all).
-include( "car.hrl" ). 


normal( From, Event, Data ) ->
    case Event of  


        { timeout, Target } -> 
            common_handler:timeout( normal, Target, Data, From );


        { crash, CrashType } ->
            NewData = crash_type( Data, CrashType ),
            flow:next( dead, NewData, From, { dead, NewData } );


        { update_front, Replacement } -> 
            common_handler:update_front( sync, Replacement, Data, From );


        { update_rear, Replacement } -> 
            common_handler:update_rear( sync, Replacement, Data, From );
            
        
        { crossing, Body } ->  
            common_handler:crossing( normal, Body, Data, From );  


        { check, Sender } -> 
            common_handler:check( normal, Sender, Data, From );


        { check_reply, Reply } ->
            utils:log( "EVENT check_reply" ),
            { _Sender, _Target, _SendingTime, RTT, Body } = Reply,

            Position = compute_position( Data ),

            if ( Position * Data#car_state.side ) =< ( Data#car_state.size / 2 ) ->

                if Data#car_state.crossing ->
                    utils:log( "Car: reach the end of the bridge" ),
                    NewData = Data#car_state{ 
                                              position = ( car_size(Data) * side(Data) / 2 ), 
                                              speed = 0, 
                                              current_time = utils:get_timestamp(),
                                              last_RTT = ( last_RTT(Data) + RTT ) / 2 
                                            },
                    flow:next( dead, NewData, From, { dead, NewData } );
                true ->
                    utils:log( "Car: reach the bridge" ),
                    NewData = Data#car_state{ 
                                              position = ( ( bridge_length(Data) + ( car_size(Data) / 2 ) ) * side(Data) ), 
                                              speed = 0, 
                                              crossing = true, 
                                              current_time = utils:get_timestamp(),
                                              last_RTT = ( last_RTT(Data) + RTT ) / 2 
                                            },
                    flow:next( leader, NewData, From, { leader, NewData } )
                end;

            true ->
                utils:log( "Car: away from the bridge" ),
                Offset = ( ( car_size(Data) / 2 ) + ( car_size(Body) / 2 ) ) * side(Data),

                MyPosition = if Data#car_state.crossing ->
                                position(Data);
                            true ->
                                position(Data) + ( bridge_length(Data) * side(Data) )
                            end,

                YourPosition = if Body#car_state.crossing ->
                                    position(Body);
                               true ->
                                    position(Body) + ( bridge_length(Body) * side(Body) )
                               end,
                utils:log( "~p MyPosition: ~p", [ position(Data), MyPosition ] ),
                utils:log( "~p YourPosition: ~p", [ position(Body), YourPosition ] ),
                Distance = ( MyPosition - YourPosition - Offset ) * side(Data),
                Speed = Distance / ( ( max_RTT(Data) ) / 1000 ),
        
                utils:log( "Car: travel with position ~p, speed ~p, distance ~p, offset ~p, lastRTT ~p", [ Position, Speed, Distance, Offset, last_RTT(Data) ] ),
        
        % continue polling
      
        NewData = Data#car_state{ 
                                  position = Position, 
                                  speed = Speed, 
                                  current_time = utils:get_timestamp(), 
                                  last_RTT = RTT 
                                },
        car_call_supervisor_api:car_call( { 
                                            log_state, 
                                            name(NewData), 
                                            undefined, 
                                            max_RTT(NewData), 
                                            NewData 
                                        } ),
        car_call_supervisor_api:car_call( { 
                                            wait, 
                                            name(NewData), 
                                            undefined, 
                                            max_RTT(NewData) - last_RTT(NewData) * 2, 
                                            max_RTT(NewData) - last_RTT(NewData) * 2 } ),
        flow:keep( NewData, From, { normal_check_reply, NewData } )
      end;


    default_behaviour ->
      utils:log( "EVENT default_behaviour" ),

      if Data#car_state.crash_type > 0 ->
        utils:log( "Postponed events, car crashed during sync" ),
        car_call_supervisor_api:car_call( { 
                                            crash, 
                                            name(Data), 
                                            name(Data), 
                                            max_RTT(Data), 
                                            crash_type(Data) 
                                        } );
      true ->
        ok
      end,

      FrontCars = front_cars( Data ), 
      if length( FrontCars ) > 0 ->

        [ First | Rest ] = FrontCars,
        if First#car_state.side == Data#car_state.side ->

            Position = compute_position( Data ),
            if ( Position * Data#car_state.side ) =< ( Data#car_state.size / 2 ) ->

                if Data#car_state.crossing ->
                    utils:log( "Car: reach the end of the bridge" ),
                    NewData = Data#car_state{ 
                                              position = ( car_size(Data) * side(Data) / 2 ), 
                                              speed = 0, 
                                              current_time = utils:get_timestamp() 
                                            },
                    flow:next( dead, NewData, From, { dead, NewData } );
                true ->
                    utils:log( "Car: reach the bridge" ),
                    NewData = Data#car_state{ 
                                              position = ( ( bridge_length(Data) + ( car_size(Data) / 2 ) ) * side(Data) ), 
                                              speed = 0, 
                                              crossing = true, 
                                              current_time = utils:get_timestamp() 
                                            },
                    flow:next( leader, NewData, From, { leader, NewData } )
                end;
            
            true ->
                utils:log( "Car: running" ),
                FrontCars = front_cars(Data),
                if length( FrontCars ) > 0 ->

                    [ FrontCar | _Rest ] = FrontCars,
                    if FrontCar#car_state.side == Data#car_state.side ->

                        utils:log( "Car: there is another car on the same side" ),
                        NewData = position( Data, Position ),
                        car_call_supervisor_api:car_call( { log_state, name(NewData), undefined, max_RTT(NewData), NewData } ),
                        car_call_supervisor_api:car_call( { check, name(NewData), name(FrontCar), max_RTT(NewData), NewData } ),
                        flow:keep( NewData, From, { normal_default_behaviour, NewData } );

                    true ->
                        utils:log( "Car: there is only a car on the opposite side of the bridge" ),
                        Speed = erlang:min( ( ( ( position(Data) * side(Data) ) - ( car_size(Data) / 2 ) ) / ( ( max_RTT(Data) ) / 1000 ) ), max_speed(Data) ),
                        utils:log( "New speed ~p", [ Speed ] ),
                        NewData = Data#car_state{ speed = Speed, position = Position, current_time = utils:get_timestamp() },
                        car_call_supervisor_api:car_call( { log_state, name(NewData), undefined, max_RTT(NewData), NewData } ),
                        car_call_supervisor_api:car_call( { wait, name(NewData), undefined, max_RTT(NewData) - last_RTT(NewData) * 2, max_RTT(NewData) - last_RTT(NewData) * 2 } ),
                        flow:keep( NewData, From, { normal_default_behaviour, NewData } )
                    end;

                true ->
                    utils:log( "Car: there is not any other car in the front queue" ),
                    Speed = erlang:min( ( ( ( position(Data) * side(Data) ) - ( car_size(Data) / 2 ) ) / ( ( max_RTT(Data) ) / 1000 ) ), max_speed(Data) ),
                    utils:log( "New speed2 ~p", [ Speed ] ),
                    NewData = Data#car_state{ speed = Speed, position = Position, current_time = utils:get_timestamp() },
                    car_call_supervisor_api:car_call( { log_state, name(NewData), undefined, max_RTT(NewData), NewData } ),
                    car_call_supervisor_api:car_call( { wait, name(NewData), undefined, max_RTT(NewData) - last_RTT(NewData) * 2, max_RTT(NewData) - last_RTT(NewData) * 2 } ),
                    flow:keep( NewData, From, { normal_default_behaviour, NewData } )
                end
            end
        end
    end;


    Event ->
        flow:ignore( normal, Event, Data, From )
    end.