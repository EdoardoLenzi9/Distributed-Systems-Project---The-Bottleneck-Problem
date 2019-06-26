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


sync_marshalling(List) ->
    Result = sync_marshallingWrapper(List),
    if Result =/= [] ->
        [[] | Rest] = Result,
        [Rest];
    true ->
        []
    end.


sync_marshallingWrapper([]) ->
    [];
sync_marshallingWrapper([First|Rest]) ->
    [sync_marshallingWrapper(Rest), [{name, First#syncEntity.name}, {side, First#syncEntity.side}, {power, First#syncEntity.power}]].