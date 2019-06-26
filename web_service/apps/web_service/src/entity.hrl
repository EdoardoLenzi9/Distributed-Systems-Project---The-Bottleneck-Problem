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
    [ {[{name, list_to_atom(First#syncEntity.name)}, {side, list_to_atom(First#syncEntity.side)}, {power, First#syncEntity.power}]} | sync_marshalling(Rest)].