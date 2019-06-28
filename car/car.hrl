%%%===================================================================
%%% macros and record definitions
%%%===================================================================

-record (carState, {    
                        % car metadata
                        name,
                        side,
                        power,
                        delta,
                        arrivalTime, 
                        adj,
                        % settings and bridge metadata 
                        turn,
                        bridgeCapacity,
                        bridgeCrossingTime
                    }).


-record (adj, { frontCars, 
                rearCars 
              }).


%%%===================================================================
%%% DTO
%%%===================================================================

-record (syncDto, {    
                        name,
                        side,
                        power
                    }).


%%%===================================================================
%%% update records
%%%===================================================================

updateAdj(Data, Adj) ->
    #carState{  
                name = Data#carState.name,
                side = Data#carState.side,
                power = Data#carState.power,
                delta = Data#carState.delta,
                arrivalTime = Data#carState.arrivalTime, 
                adj = Adj,
                bridgeCapacity =  Data#carState.bridgeCapacity,
                bridgeCrossingTime =  Data#carState.bridgeCrossingTime
            }.


updateDelta(Data, Delta) ->
    #carState{  
                name = Data#carState.name,
                side = Data#carState.side,
                power = Data#carState.power,
                delta = Delta,
                arrivalTime = Data#carState.arrivalTime, 
                adj = Data#carState.adj,
                bridgeCapacity =  Data#carState.bridgeCapacity,
                bridgeCrossingTime =  Data#carState.bridgeCrossingTime
            }.


%%%===================================================================
%%% Unmarshalling mappers (Dto -> Entity)
%%%===================================================================

unmarshalling_sync([]) ->
    [];
unmarshalling_sync([First| Rest]) ->
    { [ {<<"name">>, Name},{<<"side">>,Side},{<<"power">>,Power} ] } = First,
    [#carState{name = binary_to_list(Name), side = binary_to_list(Side), power = Power} | unmarshalling_sync(Rest)].

