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

            SyncData = if Data#car_state.delta == 0 ->
                utils:log( "Car: compute berkeley with RTT ~p", [ RTT ] ),
                CurrentTime = SendingTime, 
                PivotTime = current_time(Body),
                Delta = CurrentTime - ( PivotTime + RTT / 2 ),
                Data#car_state{ 
                                delta = Delta, 
                                last_RTT = RTT
                            };
            true ->
                utils:log( "Car: yet synchronized" ),
                last_RTT(Data, RTT ) 
            end,

            [ Rear | _Rest ] = rear_cars( Body ),
            if Rear#car_state.name == SyncData#car_state.name ->
                utils:log( "Car: No race conflict during sync" ),

                if Body#car_state.position == undefined -> 
                    utils:log( "Car: Behind not sync car" ),
                    NewData = front_cars( SyncData, [ Rear ] ),
                    car_call_supervisor_api:car_call( { 
                                                        last_adj, 
                                                        name( NewData ), 
                                                        undefined, 
                                                        max_RTT( NewData ), 
                                                        NewData 
                                                    } ),
                flow:keep( NewData, From, { sync_check_reply, NewData } );
            true ->
                Position = position(Body) + ( ( ( car_size(SyncData) / 2 ) + ( car_size(Body) / 2 ) ) * side(SyncData) ), 
                NewData = SyncData#car_state{ 
                                                speed = 0, 
                                                position = Position, 
                                                arrival_time = utils:get_timestamp(), 
                                                current_time = utils:get_timestamp(), 
                                                synchronized = true 
                                            },
                NewData2 = if Body#car_state.arrival_time > ( NewData#car_state.arrival_time + NewData#car_state.delta ) ->
                    utils:log( "Car: bad Berkley sync" ),
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
                flow:keep( NewData2, From, { sync_check_reply, NewData2 } )
            end;
            true ->
                utils:log( "Car: Looses race during sync" ),
                NewData = SyncData#car_state{ 
                                            adj = SyncData#car_state.adj#adj{ front_cars = [ Rear ] }, 
                                            arrival_time = utils:get_timestamp() 
                                            },
                car_call_supervisor_api:car_call( { 
                                                    last_adj, 
                                                    name(NewData), 
                                                    undefined, 
                                                    max_RTT(NewData), 
                                                    NewData 
                                                } ),
                flow:keep( NewData, From, { sync_check_reply, NewData } )
            end;


        { last_adj_reply, Last } ->
            utils:log( "EVENT last_adj_reply ~p", [ Last ] ),
            if Last =/= undefined ->
                car_call_supervisor_api:car_call( { 
                                                    check, 
                                                    name(Data), 
                                                    Last, 
                                                    max_RTT(Data), 
                                                    Data
                                                } ),
                flow:keep( Data, From, { sync_last_adj_reply, Data } );
            true -> 
                Position = side(Data),
                NewData = Data#car_state{ 
                                        speed = 0, 
                                        position = Position, 
                                        arrival_time = utils:get_timestamp(), 
                                        current_time = utils:get_timestamp(), 
                                        synchronized = true 
                                        },
                utils:log( "initial position: ~p, arrival time ~p", [ Position, arrival_time(NewData) ] ),
                car_call_supervisor_api:car_call( { 
                                                    adj, 
                                                    name(NewData), 
                                                    undefined, 
                                                    max_RTT(NewData), 
                                                    NewData 
                                                } ),
                flow:keep( NewData, From, { sync_last_adj_reply, NewData } )
            end;


        { adj_reply, Adj } ->
            utils:log( "EVENT adj_reply: ~p", [ Adj ] ),
            NewData = adj( Data, Adj ),
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
                                                    last_adj, 
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