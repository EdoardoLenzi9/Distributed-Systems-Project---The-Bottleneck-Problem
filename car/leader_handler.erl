-module( leader_handler ).

-compile(export_all).
-include( "car.hrl" ). 


leader( From, Event, Data ) ->
    case Event of


        { crash, CrashType } ->
            NewData = Data#car_state{ crash_type = CrashType },
            flow:next( dead, NewData, From, { dead, NewData } );


        { timeout, Target } -> 
            common_handler:timeout( leader, Target, Data, From );


        { crossing, Body } ->  
            common_handler:crossing( leader, Body, Data, From );


        { update_front, Replacement } -> 
            common_handler:update_front( sync, Replacement, Data, From );


        { update_rear, Replacement } -> 
            common_handler:update_rear( sync, Replacement, Data, From );


        { check, Sender } -> 
            common_handler:check( leader, Sender, Data, From );


        { check_reply, Reply } ->
            utils:log( "EVENT check_reply" ),
            { _Sender, _Target, _SendingTime, _RTT, Body } = Reply, 
            if Body#car_state.arrival_time > Data#car_state.arrival_time ->
                %the car can start crossing the bridge
                common_handler:propagate_crossing( Data, Body ),
                flow:next( normal, Data, From, { normal, Data } );
            true->
                %the car must wait ( has arrived after )
                flow:keep( Data, From, default_behaviour )
            end;


        default_behaviour ->
            FrontCars = Data#car_state.adj#adj.front_cars,
            if length( FrontCars ) > 0 ->
                utils:log( "There is a car on the opposite side of the bridge" ),
                [ Pivot|_Rest ] = FrontCars,
                car_call_supervisor_api:car_call( { check, name(Data), name(Pivot), max_RTT(Data), Data } ),
                flow:keep( Data, From, { leader_default_behaviour, Data } );
            true ->
                utils:log( "the car can start crossing the bridge" ),
                utils:log( "max speed ~p", [ Data#car_state.max_speed ] ),
                NewData = Data#car_state{  
                                        position = bridge_length(Data) * side(Data), 
                                        crossing = true, 
                                        speed = max_speed(Data)
                                        },
                common_handler:propagate_crossing( Data, #car_state{ arrival_time = -1, bridge_capacity = bridge_capacity(Data) } ),
                flow:next( normal, NewData, From, { normal, NewData } )
            end;

        
        Event ->
        flow:ignore( leader, Event, Data, From )
    end.