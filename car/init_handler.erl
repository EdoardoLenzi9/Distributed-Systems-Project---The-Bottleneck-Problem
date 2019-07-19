-module( init_handler ).
-compile(export_all).
-include( "car.hrl" ). 


init( State ) ->
    utils:log( "max_speed: ~p", [ max_speed(State) ] ),
    NewState = State#car_state {  
                                arrival_time = utils:get_timestamp(), 
                                current_time = utils:get_timestamp()
                            }, 
    { ok, sync, NewState }.