%%%===================================================================
%%% macros and record definitions
%%%===================================================================

-record (carState, {    
                        power,
                        bridgeCapacity,
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
                bridgeCapacity =  Data#carState.bridgeCapacity,
                name = Data#carState.name,
                delta = Data#carState.delta,
                adj = Data#carState.adj,
                timeout=Timeout }.


updateAdj(Data, Adj) ->
    #carState{  arrivalTime = Data#carState.arrivalTime, 
                bridgeCapacity =  Data#carState.bridgeCapacity,
                name = Data#carState.name,
                delta = Data#carState.delta,
                adj = Adj,
                timeout=Data#carState.timeout }.


updateDelta(Data, Delta) ->
    #carState{  arrivalTime = Data#carState.arrivalTime, 
                bridgeCapacity =  Data#carState.bridgeCapacity,
                name = Data#carState.name,
                delta = Delta,
                adj = Data#carState.adj,
                timeout=Data#carState.timeout }.


%%%===================================================================
%%% TODO management
%%%===================================================================


%% Simulate a tow truck fix after a given timeout
towTruck(Name, Timeout) ->
    timer:apply_after(Timeout, ?MODULE, sendEvent, [{global, Name}, removed]).


sendEvent(Name, Event) ->
    {list_to_atom(Name), list_to_atom(io:format("~p@~p", [Name, Name]))} ! Event,
    receive
        Response ->
            Response        
        after 500 ->
            no_response
    end. 


sendToAllAdjWrap([Car], Event) -> 
    sendEvent(Car#carState.name, Event);
sendToAllAdjWrap([FirstCar | Rest], Event) -> 
    Response = sendEvent(FirstCar#carState.name, Event),
    [sendToAllAdjWrap(Rest, Event) | Response].


sendToAllAdj(List, Event) -> 
    Responses = sendToAllAdjWrap(List, Event),
    if length(Responses) > 1 -> 
        [[First] | Rest] = Responses,
        [First | Rest ];
    true -> 
        Responses
    end.

