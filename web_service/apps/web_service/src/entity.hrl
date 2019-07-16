-record(adj_entity, {         
                        name,
                        side,
                        power,
                        size,
                        position,
                        crossing,
                        arrival_time,
                        delta,
                        state
                    }).


-record(sync_entity, {              
                        name,
                        side,
                        power,
                        size,
                        timeStamp             
                    }).


-record(settingsEntity, {
                            max_speed,
                            max_RTT,
                            tow_truck_time,
                            bridge_capacity,
                            bridge_length
                        }).


-record(newCarEntity, {
                        name,
                        side,
                        power,
                        size,
                        crash_type,
                        timeout  
                    }).


-record(carEntity, {
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
sync_marshalling([First|Rest]) ->
    [{[ {name, First#sync_entity.name}, 
        {side, First#sync_entity.side}, 
        {power, First#sync_entity.power} ]} | sync_marshalling(Rest)].


adj_marshalling([]) ->
    [];
adj_marshalling([First|Rest]) ->
    [{[ {name, First#adj_entity.name}, 
        {side, First#adj_entity.side}, 
        {power, First#adj_entity.power}, 
        {size, First#adj_entity.size}, 
        {position, First#adj_entity.position}, 
        {crossing, First#adj_entity.crossing}, 
        {arrival_time, First#adj_entity.arrival_time},
        {delta, First#adj_entity.delta},
        {state, First#adj_entity.state} ]} | adj_marshalling(Rest)].


settings_marshalling(Settings) ->

    {[  {max_speed, Settings#settingsEntity.max_speed}, 
        {max_RTT, Settings#settingsEntity.max_RTT}, 
        {tow_truck_time, Settings#settingsEntity.tow_truck_time}, 
        {bridge_capacity, Settings#settingsEntity.bridge_capacity}, 
        {bridge_length, Settings#settingsEntity.bridge_length} ]}.


car_marshalling(Car) ->
    {[ {name, Car#carEntity.name}, 
    {side, Car#carEntity.side}, 
    {power, Car#carEntity.power}, 
    {max_speed, Car#carEntity.max_speed},
    {max_RTT, Car#carEntity.max_RTT},
    {tow_truck_time, Car#carEntity.tow_truck_time},
    {bridge_capacity, Car#carEntity.bridge_capacity},
    {bridge_length, Car#carEntity.bridge_length},
    {timeout, Car#carEntity.timeout} ]}.