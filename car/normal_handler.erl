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


        { adj_reply, Adj } ->
            common_handler:adj_reply( normal, Adj, Data, From );


        { check_reply, Reply } ->
            utils:log( "EVENT check_reply" ),
            { _Sender, _Target, _SendingTime, _RTT, Body } = Reply,

            ObstaclePosition = round1f(erlang:max( 
                                            ( last_position( Body ) + ( car_size( Body)  / 2 * side( Body ) ) -
                                              ( bridge_length( Body ) * utils:bool_to_int(last_crossing( Body ) ) * utils:bool_to_int( not crossing( Data ) ) * side( Body ) ) 
                                            ) * side( Body ),
                                            
                                            0
                                         ) * side(Data)),

            utils:log("Car: body last position ~p ~p, obstacle position: ~p", [last_position(Body), last_crossing(Body), ObstaclePosition]),

            NewData = obstacle_position(Data, ObstaclePosition),

            NewData2 = if ( Body#car_state.synchronized ) ->

                car_call_supervisor_api:car_call( { 
                                                    log_state, 
                                                    name(NewData), 
                                                    undefined, 
                                                    max_RTT(NewData), 
                                                    NewData 
                                                } ),
                update_last_position(synchronized(NewData, true));
            true ->
                NewData
            end,

            car_call_supervisor_api:car_call( { 
                                                wait, 
                                                name(NewData2), 
                                                undefined, 
                                                max_RTT(NewData2), 
                                                default_behaviour
                                            } ),

            flow:keep( NewData2, From, { normal_check_reply, NewData2 } );


    default_behaviour ->
        utils:log( "EVENT default_behaviour" ),

        TravelTime = ( utils:get_timestamp() - current_time(Data) ) / 1000,

        Distance = round1f(erlang:min( ( TravelTime * max_speed( Data ) ), 
                             ( ( position(Data) - obstacle_position( Data ) ) * side(Data) - (car_size(Data) / 2)) 
                             ) * side(Data)),

        utils:log("Current position ~p, TravelTime ~p, Distance ~p", [position(Data), TravelTime, Distance]),
        NewData = position(Data, round1f(position(Data) - Distance)),
        utils:log("Jump to new position ~p", [position(NewData)]),

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

        FrontCar = if length( FrontCars ) > 0 ->
            [ First | _Rest ] = FrontCars,

            if First#car_state.side == Data#car_state.side ->
                First;
            true ->
                car_on_the_other_side
            end;

        true ->
            undefined
        end,

        if NewData#car_state.position * NewData#car_state.side =< NewData#car_state.size / 2 ->

            NewData2 = update_last_position(NewData), 
            utils:log("Car: update last position ~p ~p", [last_position(NewData2), last_crossing(NewData2)]),
            
            if NewData2#car_state.crossing ->
                utils:log( "Car: reaches the end of the bridge" ),
                flow:next( dead, NewData2, From, { dead, NewData2 } );
            true -> 
                utils:log( "Car: reaches the bridge" ),
                flow:next( leader, NewData2, From, { leader, NewData2 } )
            end;
        
        true ->

            NewData2 = if FrontCar == undefined; FrontCar == car_on_the_other_side ->
                utils:log( "Car: there is not any car in front of me" ),
            
            
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
                                                    max_RTT(NewData), 
                                                    default_behaviour 
                                                } ),

                utils:log("Car: update last position ~p ~p", [position(NewData), crossing(NewData)]),
                obstacle_position(update_last_position(synchronized(NewData, true)), 0);
            
            true -> 
                utils:log( "Car: there is a car, on the same side, in front of me" ),
            
                car_call_supervisor_api:car_call( { 
                                                    check, 
                                                    name(NewData), 
                                                    name(FrontCar), 
                                                    max_RTT(NewData), 
                                                    NewData 
                                                } ),
                NewData
            
            end,
            flow:keep( NewData2, From, { normal_default_behaviour, NewData2 } )
    
        end;

    Event ->
        flow:ignore( normal, Event, Data, From )
    end.


round1f( Float ) ->
  round( Float * 10 )/10. 