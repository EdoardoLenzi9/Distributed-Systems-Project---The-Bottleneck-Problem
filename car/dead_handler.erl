-module( dead_handler ).
-compile(export_all).
-include( "car.hrl" ). 


dead( From, Event, Data ) ->
  case Event of  

    { timeout, Target } -> 
        if Data#car_state.crash_type == 2 ->
            flow:ignore( dead, Event, Data, From );
        true ->
            common_handler:timeout( dead, Target, Data, From )
        end;


    { update_front, Replacement } -> 
        if Data#car_state.crash_type == 2 ->
            flow:ignore( dead, Event, Data, From );
        true ->
            common_handler:update_front( dead, Replacement, Data, From )
        end;


    { update_rear, Replacement } -> 
        if Data#car_state.crash_type == 2 ->
            flow:ignore( dead, Event, Data, From );
        true ->
            common_handler:update_rear( dead, Replacement, Data, From )
        end;
            
        
    { crossing, Body } ->  
        if Data#car_state.crash_type == 2 ->
            flow:ignore( dead, Event, Data, From );
        true ->
            common_handler:crossing( dead, Body, Data, From )
        end;


    { check, Sender } -> 
        if Data#car_state.crash_type == 2 ->
            flow:ignore( dead, Event, Data, From );
        true ->
            common_handler:check( dead, Sender, Data, From )
        end;
        

    tow_truck_request ->
        utils:log( "EVENT tow_truck_request" ),
        car_call_supervisor_api:car_call( { 
                                            wait, 
                                            name(Data), 
                                            undefied, 
                                            tow_truck_time(Data), 
                                            tow_truck
                                        } ),
        flow:keep( Data, From, { dead_tow_truck_request, Data } );


    tow_truck ->
        utils:log( "EVENT tow_truck" ),
        common_handler:notify_dead_and_stop( Data ),
        flow:keep( Data, From, { dead_tow_truck, Data } );


    default_behaviour ->
        utils:log( "EVENT Dead_default_behaviour" ),
        case Data#car_state.crash_type of 
            
            0 -> 
                common_handler:notify_dead_and_stop( Data ),
                flow:keep( Data, From, { dead_default_behaviour_0, Data } );
            
            1 -> 
                car_call_supervisor_api:car_call( { 
                                                    tow_truck_request, 
                                                    name(Data), 
                                                    name(Data), 
                                                    max_RTT(Data), 
                                                    tow_truck_time(Data) 
                                                } ),
                flow:keep( Data, From, { dead_default_behaviour_1, Data } );

            2 -> 
                flow:keep( Data, From, { dead_default_behaviour_2, Data } )
        end;


    Event ->
        flow:ignore( dead, Event, Data, From )
    end.