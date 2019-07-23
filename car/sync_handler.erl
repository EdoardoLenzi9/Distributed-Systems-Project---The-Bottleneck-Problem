-module( sync_handler ).

-compile(export_all).
-include( "car.hrl" ). 


sync( From, Event, Data ) ->
    case Event of 


        { crash, CrashType } ->  
            utils:log( "EVENT crash ( postpone )" ), 
            NewData = crash_type( Data, CrashType ),
            flow:keep( NewData, From, { sync_crash, NewData } );


        { timeout, Target } -> 
            common_handler:timeout( sync, Target, Data, From ); 


        { update_front, Replacement } -> 
            common_handler:update_front( sync, Replacement, Data, From );


        { update_rear, Replacement } -> 
            common_handler:update_rear( sync, Replacement, Data, From );


        { check, Sender } -> 
            common_handler:check( sync, Sender, Data, From );


        { crossing, Body } ->  
            common_handler:crossing( sync, Body, Data, From );


        { check_reply, Reply } ->
            utils:log( "EVENT check_reply" ),
            { _Sender, _Target, SendingTime, RTT, Body } = Reply,

            ObstaclePosition = erlang:max( 
                                            ( last_position( Body ) + ( car_size( Body)  / 2 * side( Body ) ) -
                                              ( bridge_length( Body ) * utils:bool_to_int(last_crossing( Body ) ) * side( Body ) ) 
                                            ) * side( Body ),
                                            
                                            car_size(Data) / 2
                                         ) * side(Data),

            utils:log( "Car: Obstacle position ~p", [ObstaclePosition] ),
            
            SyncData = if Data#car_state.delta == 0 ->
                utils:log( "Car: compute berkeley with RTT ~p", [ RTT ] ),
                CurrentTime = SendingTime, 
                PivotTime = current_time(Body),
                Delta = CurrentTime - ( PivotTime + RTT / 2 ),
                Data#car_state{ 
                                delta = Delta, 
                                last_RTT = RTT,
                                obstacle_position = ObstaclePosition
                            };
            true ->
                utils:log( "Car: yet synchronized" ),

                Data#car_state{ 
                                obstacle_position = ObstaclePosition,
                                last_RTT = RTT
                              }
            end,

            if Body#car_state.synchronized ->
                utils:log( "Car: Front car is synchronized with WS" ),

                Position = obstacle_position(SyncData) + (car_size(SyncData) / 2 * side(SyncData)), 
                NewData = SyncData#car_state{ 
                                                speed = 0, 
                                                position = Position, 
                                                arrival_time = utils:get_timestamp(), 
                                                current_time = utils:get_timestamp()
                                            },

                NewData2 = if Body#car_state.arrival_time > ( NewData#car_state.arrival_time + NewData#car_state.delta ) ->
                    utils:log( "Car: reset bad Berkley sync" ),
                    delta( NewData, 0 );
                true ->
                    utils:log( "Car: right Berkley sync" ),
                    NewData
                end,
                
                utils:log( "initial position: ~p, arrival time ~p, delta ~p", [ Position, arrival_time(NewData2), delta(NewData2) ] ),
                car_call_supervisor_api:car_call( { 
                                                    adj, 
                                                    name(NewData2), 
                                                    undefined, 
                                                    max_RTT(NewData2), 
                                                    NewData2 
                                                } ),
                flow:keep( NewData2, From, { sync_check_reply, NewData2 } );
            true ->
                utils:log( "Car: Front car is not synchronized with WS" ),
                car_call_supervisor_api:car_call( { 
                                                    check, 
                                                    name(SyncData), 
                                                    name(Body), 
                                                    max_RTT(SyncData), 
                                                    SyncData 
                                                } ),
                flow:keep( SyncData, From, { sync_check_reply, SyncData } )
            end;


        { adj_reply, Adj } ->
            utils:log( "EVENT adj_reply: ~p", [ Adj ] ),
            NewData = adj( Data, Adj ),
            utils:log( "Arrival Time: ~p", [ arrival_time(NewData) ] ),
            flow:next( normal, NewData, From, { normal, NewData } );


        default_behaviour ->
            utils:log( "EVENT default_behaviour" ),
            FrontCars = front_cars( Data ),
            if length( FrontCars ) > 0 ->
                [ Pivot | _Rest ] = FrontCars,
                utils:log( "Car: there is a car in the front queue, send a check independently from its side" ),
                car_call_supervisor_api:car_call( { 
                                                    check, 
                                                    name(Data), 
                                                    name(Pivot), 
                                                    max_RTT(Data), 
                                                    Data
                                                } );
            true ->
                utils:log( "Car: there is not any other car in the front queue" ),
                car_call_supervisor_api:car_call( { 
                                                    adj, 
                                                    name(Data), 
                                                    undefined, 
                                                    max_RTT(Data), 
                                                    Data 
                                                } )
            end,
            flow:keep( Data, From, { sync_default_behaviour, Data } );

        
        Event ->
            flow:ignore( sync, Event, Data, From )
        end.