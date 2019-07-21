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
            { _Sender, _Target, _SendingTime, _RTT, Body } = Reply,

            ObstaclePosition = erlang:min( 
                                           position(Body),
                                           car_size(Data) / 2 * side(Data)
                                         ),
            NewData = obstacle_position(Data, ObstaclePosition),

            if ( Body#car_state.synchronized ) ->

                car_call_supervisor_api:car_call( { 
                                                    log_state, 
                                                    name(NewData), 
                                                    undefined, 
                                                    max_RTT(NewData), 
                                                    NewData 
                                                } );
            true ->
                ok
            end;


    default_behaviour ->
        utils:log( "EVENT default_behaviour" ),

        TravelTime = ( utils:get_timestamp() - current_time(Data) ) / 1000,

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

        Distance = erlang:min( ( TravelTime * max_speed( Data ) ), 
                             ( ( position(Data) - obstacle_position( Data ) ) * side(Data) ) 
                             ) * side(Data),

        utils:log("Current position ~p, TravelTime ~p, Distance ~p", [position(Data), TravelTime, Distance]),
        NewData = position(Data, position(Data) - Distance),
        utils:log("Jump to new position ~p", [position(NewData)]),

        if FrontCar == undefined; FrontCar == car_on_the_other_side ->
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
                                                undefied, 
                                                max_RTT(NewData), 
                                                max_RTT(NewData) 
                                            } );

        true -> 

            utils:log( "Car: there is a car, on the same side, in front of me" ),

            car_call_supervisor_api:car_call( { 
                                                check, 
                                                name(NewData), 
                                                name(FrontCar), 
                                                max_RTT(NewData), 
                                                NewData 
                                            } )

        end,

        if NewData#car_state.position * NewData#car_state.side =< NewData#car_state.size / 2 ->

            if Data#car_state.crossing ->
                utils:log( "Car: reaches the end of the bridge" ),
                flow:next( dead, NewData, From, { dead, NewData } );
            true -> 
                utils:log( "Car: reaches the bridge" ),
                flow:next( leader, NewData, From, { leader, NewData } )
            end;
        
        true ->

            flow:keep( NewData, From, { normal_default_behaviour, NewData } )
    
        end;

    Event ->
        flow:ignore( normal, Event, Data, From )
    end.