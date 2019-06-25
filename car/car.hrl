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
                        % bridge metadata 
                        bridgeCapacity,
                        bridgeCrossingTime
                    }).


-record (adj, { frontCars, 
                rearCars 
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


%%%===================================================================
%%% web service calls
%%%===================================================================

getAdjacencies() -> 
    #adj{ frontCars = [], rearCars = []}.