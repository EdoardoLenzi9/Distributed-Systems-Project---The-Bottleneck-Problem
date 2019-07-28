-record(adj_entity, {         
                        name,
                        host,
                        ip,
                        side,
                        power,
                        size,
                        position,
                        crossing,
                        arrival_time,
                        delta,
                        state,
                        crash_type
                    }).


-record(sync_entity, {              
                        name,
                        side,
                        power,
                        size,
                        time_stamp,
                        front_car,
                        rear_car             
                    }).


-record(host_entity, {              
                        id,
                        host,
                        ip,
                        password,
                        number_of_cars
}).


-record(settings_entity, {
                            process_visibility,
                            max_speed,
                            max_RTT,
                            tow_truck_time,
                            bridge_capacity,
                            bridge_length
                        }).


-record(new_car_entity, {
                        host,
                        port,
                        name,
                        side,
                        power,
                        size,
                        crash_type,
                        timeout  
                    }).


-record(car_entity, {
                        name,
                        side,
                        power,
                        size,
                        max_speed,
                        max_RTT,
                        tow_truck_time,
                        bridge_capacity,
                        bridge_length,
                        timeout  
                    }).

            
%%%===================================================================
%%% Marshalling (Entity -> Dto)
%%%===================================================================

sync_marshalling([]) ->
    [];
sync_marshalling(First) ->
    [{[ {name, First#sync_entity.name}, 
        {side, First#sync_entity.side}, 
        {power, First#sync_entity.power} ]}].


adj_marshalling([]) ->
    [];
adj_marshalling([First|Rest]) ->
    [{[ {name, First#adj_entity.name}, 
        {host, First#adj_entity.host}, 
        {ip, First#adj_entity.ip}, 
        {side, First#adj_entity.side}, 
        {power, First#adj_entity.power}, 
        {size, First#adj_entity.size}, 
        {position, First#adj_entity.position}, 
        {crossing, First#adj_entity.crossing}, 
        {arrival_time, First#adj_entity.arrival_time},
        {delta, First#adj_entity.delta},
        {state, First#adj_entity.state},
        {crash_type, First#adj_entity.crash_type} ]} | adj_marshalling(Rest)].


settings_marshalling(Settings) ->
    {[  
        {process_visibility, Settings#settings_entity.process_visibility}, 
        {max_speed, Settings#settings_entity.max_speed}, 
        {max_RTT, Settings#settings_entity.max_RTT}, 
        {tow_truck_time, Settings#settings_entity.tow_truck_time}, 
        {bridge_capacity, Settings#settings_entity.bridge_capacity}, 
        {bridge_length, Settings#settings_entity.bridge_length} ]}.


car_marshalling(Car) ->
    {[ {name, Car#car_entity.name}, 
    {side, Car#car_entity.side}, 
    {power, Car#car_entity.power}, 
    {max_speed, Car#car_entity.max_speed},
    {max_RTT, Car#car_entity.max_RTT},
    {tow_truck_time, Car#car_entity.tow_truck_time},
    {bridge_capacity, Car#car_entity.bridge_capacity},
    {bridge_length, Car#car_entity.bridge_length},
    {timeout, Car#car_entity.timeout} ]}.


last_adj_marshalling(Car, Side) ->
    if Car =/= undefined ->
        if Car#adj_entity.side == Side ->
            {[ {name, Car#adj_entity.name} ]};
        true ->
            {[ {name, undefined} ]}
        end;    
    true ->
        {[ {name, Car} ]}
    end.