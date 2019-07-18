%%%===================================================================
%%% macros and record definitions
%%%===================================================================

-record (car_state, {    
                        % car metadata
                        name,
                        side,
                        power,
                        size,
                        speed,
                        position,
                        crossing,
                        synchronized,
                        crash_type,     % 0 normal, 1 broken, 2 dead
                        delta,
                        arrival_time, 
                        current_time,
                        adj,
                        state,
                        % settings and bridge metadata 
                        host,
                        port,
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
                side = Side, 
                power = Power } | unmarshalling_sync(Rest)].


unmarshalling_adj([ Front | Rest ]) ->
    utils:log("unmarshalling_adj_wrapper1"),
    [Back] = Rest,
    #adj{ front_cars = unmarshalling_adj_wrapper(Front), rear_cars = unmarshalling_adj_wrapper(Back) }.


unmarshalling_adj_wrapper([]) ->
    [];
unmarshalling_adj_wrapper([First| Rest]) ->
    utils:log("unmarshalling_adj_wrapper1"),
    { [ {<<"name">>, Name}, 
        {<<"side">>,Side},
        {<<"power">>,Power},
        {<<"size">>,Size},
        {<<"position">>,Position},
        {<<"crossing">>,Crossing},
        {<<"arrival_time">>,ArrivalTime},
        {<<"delta">>,Delta},
        {<<"state">>,State},
        {<<"crash_type">>, CrashType} ] } = First,
    utils:log("unmarshalling_adj_wrapper2"),
    [#car_state{    name = utils:binary_to_atom(Name), 
                    side = Side, 
                    power = Power,
                    size = Size,
                    position = Position,
                    crossing = Crossing,
                    arrival_time = ArrivalTime,
                    delta = Delta,
                    state = utils:binary_to_atom(State),
                    crash_type = CrashType} | unmarshalling_adj_wrapper(Rest)].    


unmarshalling_last_adj(Last) ->
    utils:log("unmarshalling_last_adj"),
    { [ {<<"name">>, Name} ] } = Last,
    utils:binary_to_atom(Name).