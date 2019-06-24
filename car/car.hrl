%%%===================================================================
%%% macros and record definitions
%%%===================================================================

-record (carState, {    name,
                        arrivalTime, 
                        delta,
                        timeout, 
                        adj }).


-record (adj, { frontCars, 
                rearCars }).


%%%===================================================================
%%% update records
%%%===================================================================

updateTimeout(Data, Timeout) ->
    #carState{ arrivalTime = Data#carState.arrivalTime, 
            name = Data#carState.name,
            delta = Data#carState.delta,
            adj = Data#carState.adj,
            timeout=Timeout }.


updateAdj(Data, Adj) ->
    #carState{ arrivalTime = Data#carState.arrivalTime, 
            name = Data#carState.name,
            delta = Data#carState.delta,
            adj = Adj,
            timeout=Data#carState.timeout }.


updateDelta(Data, Delta) ->
    #carState{ arrivalTime = Data#carState.arrivalTime, 
            name = Data#carState.name,
            delta = Delta,
            adj = Data#carState.adj,
            timeout=Data#carState.timeout }.