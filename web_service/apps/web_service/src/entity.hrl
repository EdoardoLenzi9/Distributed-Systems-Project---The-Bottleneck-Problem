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