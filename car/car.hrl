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
                        last_RTT,
                        obstacle_position,
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
%%% Getter and setter
%%%===================================================================

name( Data, Value ) ->
    Data#car_state{ name = Value }.
name( Data ) ->
    Data#car_state.name.


side( Data ) ->
    Data#car_state.side.


power( Data ) ->
    Data#car_state.power.


car_size( Data ) ->
    Data#car_state.size.


speed( Data, Value ) ->
    Data#car_state{ speed = Value }.
speed( Data ) ->
    Data#car_state.speed.


position( Data, Value ) ->
    Data#car_state{ position = Value }.
position( Data ) ->
    Data#car_state.position.


crossing( Data, Value ) ->
    Data#car_state{ crossing = Value }.
crossing( Data ) ->
    Data#car_state.crossing.


synchronized( Data, Value ) ->
    Data#car_state{ synchronized = Value }.
synchronized( Data ) ->
    Data#car_state.synchronized.


crash_type( Data, Value ) ->
    Data#car_state{ crash_type = Value }.
crash_type( Data ) ->
    Data#car_state.crash_type.


delta( Data, Value ) ->
    Data#car_state{ delta = Value }.
delta( Data ) ->
    Data#car_state.delta.


arrival_time( Data, Value ) ->
    Data#car_state{ arrival_time = Value }.
arrival_time( Data ) ->
    Data#car_state.arrival_time.


current_time( Data, Value ) ->
    Data#car_state{ current_time = Value }.
current_time( Data ) ->
    Data#car_state.current_time.


adj( Data, Value ) ->
    Data#car_state{ adj = Value }.
adj( Data ) ->
    Data#car_state.adj.


front_cars( Data, Value ) ->
    Data#car_state{ adj = Data#car_state.adj#adj{ front_cars = Value } }.
front_cars( Data ) ->
    Data#car_state.adj#adj.front_cars.


rear_cars( Data, Value ) ->
    Data#car_state{ adj = Data#car_state.adj#adj{ rear_cars = Value } }.
rear_cars( Data ) ->
    Data#car_state.adj#adj.rear_cars.


state( Data, Value ) ->
    Data#car_state{ state = Value }.
state( Data ) ->
    Data#car_state.state.


last_RTT( Data, Value ) ->
    Data#car_state{ last_RTT = Value }.
last_RTT( Data ) ->
    Data#car_state.last_RTT.


obstacle_position( Data, Value ) ->
    Data#car_state{ obstacle_position = Value }.
obstacle_position( Data ) ->
    Data#car_state.obstacle_position.


host( Data ) ->
    Data#car_state.host.


port( Data ) ->
    Data#car_state.port.


bridge_capacity( Data, Value ) ->
    Data#car_state{ bridge_capacity = Value }.
bridge_capacity( Data ) ->
    Data#car_state.bridge_capacity.


bridge_length( Data ) ->
    Data#car_state.bridge_length.


max_speed( Data ) ->
    Data#car_state.max_speed.


tow_truck_time( Data ) ->
    Data#car_state.tow_truck_time.


max_RTT( Data ) ->
    Data#car_state.max_RTT.


%%%===================================================================
%%% Unmarshalling mappers (Dto -> Entity)
%%%===================================================================

unmarshalling_sync([]) ->
    [];
unmarshalling_sync([First]) ->
    { [ {<<"name">>, Name},{<<"side">>,Side},{<<"power">>,Power} ] } = First,
    [ #car_state{ name = utils:binary_to_atom(Name), 
                  side = Side, 
                  power = Power } ].


unmarshalling_adj([ Front | Rest ]) ->
    utils:log("unmarshalling_adj_wrapper"),
    [Back] = Rest,
    #adj{ front_cars = unmarshalling_adj_wrapper(Front), rear_cars = unmarshalling_adj_wrapper(Back) }.


unmarshalling_adj_wrapper([]) ->
    [];
unmarshalling_adj_wrapper([First| Rest]) ->
    utils:log("unmarshalling_adj_wrapper"),
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