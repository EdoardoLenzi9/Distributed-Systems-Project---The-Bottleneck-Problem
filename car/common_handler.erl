%% @author Edoardo Lenzi, Talissa Dreossi
%% @copyright GPL-3
%% @version 1.0.0


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
		if FrontCar#car_state.side == Data#car_state.side ->
	      	utils:log( "Car: Send update rear to front car ~p with body ~p ~n", [ name( FrontCar ), RearCar ] ),
    	  	car_call_supervisor_api:car_call( { 
												update_rear, 
												name( Data ), 
												name( FrontCar ), 
												max_RTT( Data ), 
												RearCar 
											} );
		true ->
			utils:log( "Car: Send update front to front car ~p with body ~p ~n", [ name( FrontCar ), RearCar ] ),
			car_call_supervisor_api:car_call( { 
											  update_front, 
											  name( Data ), 
											  name( FrontCar ), 
											  max_RTT( Data ), 
											  RearCar 
										  } )
		end;
    true ->
      	ok
    end,
    
	if RearCar =/= [ ] ->
		utils:log( "Car: Send update front to rear car ~p with body ~p ~n", [ name( RearCar ), FrontCar ] ),
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
	
	NewData = state( Data, stop ),
    car_call_supervisor_api:car_call( { 
                                        stop, 
                                        name( NewData ), 
                                        name( NewData ), 
                                        max_RTT( NewData ), 
                                        state( NewData, stop ) 
                                  	} ).


timeout( State, Target, Data, From ) ->
	utils:log( "EVENT timeout" ), 
	car_call_supervisor_api:car_call( { 
										tow_truck_request, 
										name( Data ), 
										Target, 
										tow_truck_time( Data ), 
										Data 
									} ),
	flow:keep_ignore( Data, From, { list_to_atom( string:concat( atom_to_list( State ),"_timeout" ) ), Data } ).
		

update_front( State, Replacement, Data, From ) ->
	utils:log( "EVENT update_front" ), 
	NewData = if Replacement == [ ] ->
		front_cars( Data, [ ] );
	true -> 
		front_cars( Data, [ Replacement ] )
	end,
	car_call_supervisor_api:car_call( { 
										wait, 
										name( Data ),
										name( Data ),
										0, 
										default_behaviour 
									} ),
	NewData2 = obstacle_position(NewData, safe_obstacle_position(NewData)),
  	flow:keep_ignore( NewData2, From, { list_to_atom( string:concat( atom_to_list( State ),"_update_front" ) ), NewData2 } ).


safe_obstacle_position(Data) ->
	utils:log("Apply Safe Obstacle Position"),
	position(Data) - ( car_size( Data ) / 2 * side( Data ) ).


update_rear( State, Replacement, Data, From ) ->
	utils:log( "EVENT update_rear" ), 
	NewData = if Replacement == [ ] ->
		rear_cars( Data, [ ] );
	true -> 
		rear_cars( Data, [ Replacement ] )
	end,
	car_call_supervisor_api:car_call( { 
										wait, 
										name( Data ),
										name( Data ),
										0, 
										default_behaviour 
									} ),
	flow:keep_ignore( NewData, From, { list_to_atom( string:concat( atom_to_list( State ),"_update_rear" ) ), NewData } ).


check( State, Sender, Data, From ) -> 
	utils:log( "EVENT check" ), 

	TargetQueue = if Sender#car_state.side == Data#car_state.side ->
		lists:reverse( rear_cars( Data ) );
	true ->
		front_cars( Data )
	end,

	NewData = if length( TargetQueue ) > 0 ->
		[ First | _Rest ] = TargetQueue,
		
		if First#car_state.name == Sender#car_state.name ->
			utils:log( "Car: Rear adj unchanged" ),
			Data;

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
		if Sender#car_state.side == Data#car_state.side ->
			utils:log( "Car: Rear adj update from [ ] to ~p", [ Sender ] ),
			rear_cars( Data, [ Sender ] );
		true ->
			utils:log( "Car: Front adj update from [ ] to ~p", [ Sender ] ),
			front_cars( Data, [ Sender ] )
		end
	end,

	flow:keep_ignore( NewData, From, { list_to_atom( string:concat( atom_to_list( State ),"_check" ) ), NewData } ).


adj_reply( State, Adj, Data, From ) ->
	utils:log( "EVENT adj_reply ~p", [ Adj ] ), 
	NewData = adj( Data, Adj ),
	NewData2 = obstacle_position( NewData, safe_obstacle_position( NewData ) ),
	car_call_supervisor_api:car_call( { 
										wait, 
										name( NewData2 ),
										name( NewData2 ),
										0, 
										default_behaviour 
									} ),
	flow:keep_ignore( NewData2, From, { list_to_atom( string:concat( atom_to_list( State ), "_adj_reply" ) ), NewData2 } ).


crossing( State, Body, Data, From ) ->
  	utils:log( "EVENT crossing" ), 

	if Body#car_state.arrival_time >= State#car_state.arrival_time; Body#car_state.arrival_time < 0 ->
		utils:log( "Car: can cross the bridge, propagate" ),
		propagate_crossing( Data, Body ),
	
		if Data#car_state.crossing ->
			flow:keep_ignore( Data, From, { list_to_atom( string:concat( atom_to_list( State ), "_crossing" ) ), Data } );
		true ->
			Position = position( Data ) + ( bridge_length( Data ) * side( Data ) ),
			NewData = Data#car_state{ 
									  crossing = true, 
									  position = Position
									},
			NewData2 = obstacle_position(NewData, safe_obstacle_position(NewData)),
			flow:keep_ignore( NewData2, From, { list_to_atom( string:concat( atom_to_list( State ), "_crossing" ) ), NewData2 } )
		end;
	true ->
		flow:keep_ignore( Data, From, { list_to_atom( string:concat( atom_to_list( State ), "_crossing" ) ), Data } )
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