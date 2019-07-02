%%%===================================================================
%%% macros and record definitions
%%%===================================================================

-record (car_state, {    
                        % car metadata
                        name,
                        side,
                        power,
                        speed,
                        position,
                        crossing,
                        delta,
                        sending_time,
                        arrival_time, 
                        current_time,
                        adj,
                        state,
                        % settings and bridge metadata 
                        turn,
                        bridge_capacity,
                        bridge_length,
                        max_speed,
                        tow_truck_time,
                        max_RTT
                    }).


-record (adj, { front_cars, 
                rear_cars 
              }).


%%%===================================================================
%%% DTO
%%%===================================================================

-record (syncDto, {    
                        name,
                        side,
                        power
                    }).


-record (env, {    
                host,
                max_speed,
                turn,
                bridge_capacity,
                bridge_length,
                tow_truck_time,
                max_RTT
            }).


%%%===================================================================
%%% Unmarshalling mappers (Dto -> Entity)
%%%===================================================================

unmarshalling_sync([]) ->
    [];
unmarshalling_sync([First| Rest]) ->
    { [ {<<"name">>, Name},{<<"side">>,Side},{<<"power">>,Power} ] } = First,
    [#car_state{ name = utils:binary_to_atom(Name), 
                side = utils:binary_to_atom(Side), 
                power = Power } | unmarshalling_sync(Rest)].


unmarshalling_adj([ [], [] ]) ->
    #adj{ front_cars = [], rear_cars = [] };
unmarshalling_adj([ [Front], [Back] ]) ->
    #adj{ front_cars = unmarshalling_adj_wrapper(Front), rear_cars = unmarshalling_adj_wrapper(Back) }.


unmarshalling_adj_wrapper([]) ->
    [];
unmarshalling_adj_wrapper([First| Rest]) ->
    { [ {<<"name">>, Name}, 
        {<<"side">>,Side},
        {<<"power">>,Power},
        {<<"arrival_time">>,ArrivalTime},
        {<<"delta">>,Delta},
        {<<"state">>,State} ] } = First,
    [#car_state{    name = utils:binary_to_atom(Name), 
                    side = utils:binary_to_atom(Side), 
                    power = Power,
                    arrival_time = ArrivalTime,
                    delta = Delta,
                    state = utils:binary_to_atom(State)} | unmarshalling_adj_wrapper(Rest)].    