-record(adjEntity, {         
                        name,
                        side,
                        power,
                        arrivalTime,
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
                            bridgeCapacity,
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
                        bridgeCapacity, 
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
        {arrivalTime, First#adjEntity.arrivalTime},
        {delta, First#adjEntity.delta},
        {state, First#adjEntity.state} ]} | adj_marshalling(Rest)].


settings_marshalling(Settings) ->
    {[  {turn, Settings#settingsEntity.turn}, 
        {bridgeCapacity, Settings#settingsEntity.bridgeCapacity}, 
        {bridgeCrossingTime, Settings#settingsEntity.bridgeCrossingTime} ]}.


car_marshalling(Car) ->
    {[ {name, Car#carEntity.name}, 
    {side, Car#carEntity.side}, 
    {power, Car#carEntity.power}, 
    {turn, Car#carEntity.turn},
    {bridgeCapacity, Car#carEntity.bridgeCapacity},
    {bridgeCrossingTime, Car#carEntity.bridgeCrossingTime},
    {timeout, Car#carEntity.timeout} ]}.