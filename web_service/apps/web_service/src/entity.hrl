-record(adjEntity, {         
                        name,
                        state, 
                        side,
                        power,
                        delta,
                        arrivalTime
                    }).


-record(syncEntity, {              
                        timeStamp,             
                        name,
                        side,
                        power
                    }).


-record(settings, {
                    turn,
                    bridgeCapacity,
                    bridgeCrossingTime
                }).


%%%===================================================================
%%% Unmarshalling mappers (Dto -> Entity)
%%%===================================================================

sync_marshalling([]) ->
    [];
sync_marshalling([First|Rest]) ->
    [ {[{name, First#syncEntity.name}, {side, First#syncEntity.side}, {power, First#syncEntity.power}]} | sync_marshalling(Rest)].