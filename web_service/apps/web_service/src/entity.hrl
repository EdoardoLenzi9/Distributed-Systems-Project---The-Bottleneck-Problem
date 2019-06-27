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


%%%===================================================================
%%% Marshalling (Entity -> Dto)
%%%===================================================================

sync_marshalling([]) ->
    [];
sync_marshalling([First|Rest]) ->
    [{[ {name, list_to_atom(First#syncEntity.name)}, 
        {side, list_to_atom(First#syncEntity.side)}, 
        {power, First#syncEntity.power} ]} | sync_marshalling(Rest)].


adj_marshalling([]) ->
    [];
adj_marshalling([First|Rest]) ->
    [{[ {name, list_to_atom(First#adjEntity.name)}, 
        {side, list_to_atom(First#adjEntity.side)}, 
        {power, First#adjEntity.power}, 
        {arrivalTime, First#adjEntity.arrivalTime},
        {delta, First#adjEntity.delta},
        {state, list_to_atom(First#adjEntity.state)} ]} | adj_marshalling(Rest)].


settings_marshalling(Settings) ->
    {[  {turn, Settings#settingsEntity.turn}, 
        {bridgeCapacity, Settings#settingsEntity.bridgeCapacity}, 
        {bridgeCrossingTime, Settings#settingsEntity.bridgeCrossingTime} ]}.