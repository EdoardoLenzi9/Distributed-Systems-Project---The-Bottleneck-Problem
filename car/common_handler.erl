-module( common_handler ).

-compile( export_all ).
-include( "car.hrl" ). 

%%%===================================================================
%%% Utility functions
%%%===================================================================


notify_dead_and_stop( Data ) ->
	utils:log( "Notify dead ~p ~n", [ adj( Data ) ] ),
	
    FrontCar = if length( Data#car_state.adj#adj.front_cars ) > 0 ->
      	[ First | _Rest ] = front_cars( Data ),
      	First;
    true -> 
      	[ ]
	end,
	
    RearCar = if length( Data#car_state.adj#adj.rear_cars ) > 0 ->
      	[ Last | _ ] = lists:reverse( rear_cars( Data ) ),
      	Last;
    true -> 
      [ ]
	end,
	
    if FrontCar =/= [ ] ->
      	utils:log( "Send update rear to front car ~p with body ~p ~n", [ name( FrontCar ), RearCar ] ),
      	car_call_supervisor_api:car_call( { 
											update_rear, 
											name( Data ), 
											name( FrontCar ), 
											max_RTT( Data ), 
											RearCar 
										} );
    true ->
      	ok
    end,
    
	if RearCar =/= [ ] ->
		utils:log( "Send update front to rear car ~p with body ~p ~n", [ name( RearCar ), FrontCar ] ),
      	car_call_supervisor_api:car_call( { 	
											update_front, 
											name( Data), 
											name( RearCar ), 
											max_RTT( Data ), 
											FrontCar 
										} );
    true ->
      	ok
	end,
	
	NewData = state(Data, stop),
    car_call_supervisor_api:car_call( { 
                                        stop, 
                                        name( NewData ), 
                                        name( NewData ), 
                                        max_RTT( NewData ), 
                                        state( NewData, stop ) 
                                  	} ).


%round1f( Float ) ->
%  round( Float * 10 )/10. 


timeout( State, Target, Data, From ) ->
	utils:log( "EVENT timeout" ), 
	car_call_supervisor_api:car_call( { 
										tow_truck_request, 
										name( Data ), 
										Target, 
										max_RTT( Data ), 
										tow_truck_time( Data ) 
									} ),
	flow:keep( Data, From, { list_to_atom( string:concat( atom_to_list( State ),"_timeout" ) ), Data } ).
		

update_front( State, Replacement, Data, From ) ->
	utils:log( "EVENT update_front" ), 
	NewData = front_cars( Data, Replacement ),
	car_call_supervisor_api:car_call( { 
										default_behaviour, 
										name( Data ),
										name( Data ),
										max_RTT( Data ), 
										{ } 
									} ),
  	flow:keep( NewData, From, { list_to_atom( string:concat( atom_to_list( State ),"_update_front" ) ), NewData } ).


update_rear( State, Replacement, Data, From ) ->
	utils:log( "EVENT update_rear" ), 
	NewData = rear_cars( Data, Replacement ),
	car_call_supervisor_api:car_call( { 
										default_behaviour, 
										name( Data ),
										name( Data ),
										max_RTT( Data ), 
										{ } 
									} ),
	flow:keep( NewData, From, { list_to_atom( string:concat( atom_to_list( State ),"_update_rear" ) ), NewData } ).


check( State, Sender, Data, From ) -> 
	utils:log( "EVENT check" ), 

	NewData = if length( Data#car_state.adj#adj.rear_cars ) > 0 ->
		[ Rear | _Rest ] = rear_cars( Data ),
		
		if Rear#car_state.name == Sender#car_state.name ->
			utils:log( "Car: Rear adj unchanged" ),
			current_time( Data, utils:get_timestamp() );

		true ->
		
			if Sender#car_state.state == sync ->
				utils:log( "Car: detected conflict during sync" ),      
				current_time( Data, utils:get_timestamp() );

			true ->
				utils:log( "Car: Rear adj update from ~p to ~p", [ rear_cars( Data ), Sender ] ),
				rear_cars( Data, [ Sender ] )
			end
		end;
	true ->
		utils:log( "Car: Rear adj update from [ ] to ~p", [ Sender ] ),
		rear_cars( Data, [ Sender ] )
	end,

	flow:keep( NewData, From, { list_to_atom( string:concat( atom_to_list( State ),"_check" ) ), NewData } ).


crossing( State, Body, Data, From ) ->
  	utils:log( "EVENT crossing" ), 

	if Body#car_state.arrival_time >= State#car_state.arrival_time; Body#car_state.arrival_time < 0 ->
		utils:log( "Car: can cross the bridge, propagate" ),
		propagate_crossing( Data, Body ),
	
		if Data#car_state.crossing ->
			flow:keep( Data, From, { list_to_atom( string:concat( atom_to_list( State ),"_crossing" ) ), Data } );
		true ->
			NewData = Data#car_state{ 
									  crossing = true, 
									  position = position( Data ) + ( bridge_length( Data ) * side( Data ) )
									},
			flow:keep( NewData, From, { list_to_atom( string:concat( atom_to_list( State ),"_crossing" ) ), NewData } )
		end
	end.


propagate_crossing( Data, Body ) -> 
	RearCars = utils:first_elements( rear_cars( Data ), bridge_length( Data ) ),
	propagate_crossing_wrapper( Data, RearCars, Body ).


propagate_crossing_wrapper( Data, RearCars, Body ) -> 
	if length( RearCars ) > 0, Body#car_state.bridge_capacity > 0 ->
		[ Pivot | Rest ] = RearCars,
		NewBody = bridge_capacity( Body, bridge_capacity( Body ) - 1 ),
		car_call_supervisor_api:car_call( { 
											crossing, 
											name( Data ), 
											name( Pivot ), 
											max_RTT( Data ), 
											NewBody 
										} ),
		propagate_crossing_wrapper( Data, Rest, NewBody );
	true ->
		utils:log( "Propagation ends" )
	end.