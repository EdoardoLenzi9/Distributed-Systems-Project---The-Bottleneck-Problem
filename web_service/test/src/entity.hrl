-record(adjEntity, {         
                        name,
                        side,
                        power,
                        arrival_time,
                        delta,
                        state
                    }).


-record(syncEntity, {              
                        name,
                        side,
                        power,
                        timeStamp             
                    }).


-record(settingsEntity, {
                            turn,
                            bridge_capacity,
                            bridgeCrossingTime
                        }).


-record(newCarEntity, {
                        name,
                        side,
                        power,
                        timeout  
                    }).


-record(carEntity, {
                        name,
                        side,
                        power,
                        turn, 
                        bridge_capacity, 
                        bridgeCrossingTime, 
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
        {arrival_time, First#adjEntity.arrival_time},
        {delta, First#adjEntity.delta},
        {state, First#adjEntity.state} ]} | adj_marshalling(Rest)].


settings_marshalling(Settings) ->
    {[  {turn, Settings#settingsEntity.turn}, 
        {bridge_capacity, Settings#settingsEntity.bridge_capacity}, 
        {bridgeCrossingTime, Settings#settingsEntity.bridgeCrossingTime} ]}.


car_marshalling(Car) ->
    {[ {name, Car#carEntity.name}, 
    {side, Car#carEntity.side}, 
    {power, Car#carEntity.power}, 
    {turn, Car#carEntity.turn},
    {bridge_capacity, Car#carEntity.bridge_capacity},
    {bridgeCrossingTime, Car#carEntity.bridgeCrossingTime},
    {timeout, Car#carEntity.timeout} ]}.