-record(adjEntity, {         
                        name,
                        side,
                        power,
                        size,
                        arrival_time,
                        delta,
                        state
                    }).


-record(syncEntity, {              
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
    [{[ {name, First#syncEntity.name}, 
        {side, First#syncEntity.side}, 
        {power, First#syncEntity.power} ]} | sync_marshalling(Rest)].


adj_marshalling([]) ->
    [];
adj_marshalling([First|Rest]) ->
    [{[ {name, First#adjEntity.name}, 
        {side, First#adjEntity.side}, 
        {power, First#adjEntity.power}, 
        {size, First#adjEntity.size}, 
        {arrival_time, First#adjEntity.arrival_time},
        {delta, First#adjEntity.delta},
        {state, First#adjEntity.state} ]} | adj_marshalling(Rest)].


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