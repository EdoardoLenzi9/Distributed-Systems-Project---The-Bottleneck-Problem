%%%===================================================================
%%% macros and record definitions
%%%===================================================================

-record (carState, {    
                        power,
                        bridgeLength,
                        name,
                        arrivalTime, 
                        delta,
                        timeout, 
                        adj 
                    }).


-record (adj, { frontCars, 
                rearCars }).


%%%===================================================================
%%% update records
%%%===================================================================

updateTimeout(Data, Timeout) ->
    #carState{  arrivalTime = Data#carState.arrivalTime, 
                bridgeLength =  Data#carState.bridgeLength,
                name = Data#carState.name,
                delta = Data#carState.delta,
                adj = Data#carState.adj,
                timeout=Timeout }.


updateAdj(Data, Adj) ->
    #carState{  arrivalTime = Data#carState.arrivalTime, 
                bridgeLength =  Data#carState.bridgeLength,
                name = Data#carState.name,
                delta = Data#carState.delta,
                adj = Adj,
                timeout=Data#carState.timeout }.


updateDelta(Data, Delta) ->
    #carState{  arrivalTime = Data#carState.arrivalTime, 
                bridgeLength =  Data#carState.bridgeLength,
                name = Data#carState.name,
                delta = Delta,
                adj = Data#carState.adj,
                timeout=Data#carState.timeout }.


%%%===================================================================
%%% TODO management
%%%===================================================================

notifyCrossing(0, _List) ->
    ok;
notifyCrossing(BridgeLength, [First|Rest]) ->
    sendEvent(First#carState.name, crossing),
    notifyCrossing(BridgeLength - 1, [Rest]).


sendEvent(Name, Event) ->
    {list_to_atom(Name), list_to_atom(io:format("~p@~p", [Name, Name]))} ! Event.