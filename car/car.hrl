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
    {Name, list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Name))} ! Event,
    receive
        Response ->
            Response        
        after 500 ->
            no_response
    end. 

sendToAllAdjWrap([], _Event) -> 
    [];
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
%%% DTO
%%%===================================================================

-record (syncDto, {    
                        name,
                        side,
                        power
                    }).



%%%===================================================================
%%% web service calls
%%%===================================================================

getSyncAdj(Name, Side, Power) -> 
    Content = {[{name, Name}, {side, Side}, {power, Power}]},
    http_client:call(post, "/car/sync", Content, car, unmarshalling_sync).


getAdj(Name, Side, Power) -> 
    Content = {[{name, Name}, {side, Side}, {power, Power}]},
    http_client:call(post, "/car/sync", Content, car, unmarshalling_sync).


unmarshalling_sync([]) ->
    [];
unmarshalling_sync([First| Rest]) ->
    { [ {<<"name">>, Name},{<<"side">>,Side},{<<"power">>,Power} ] } = First,
    [#carState{name = binary_to_list(Name), side = binary_to_list(Side), power = Power} | unmarshalling_sync(Rest)].

%%%===================================================================
%%% Unmarshalling mappers (Dto -> Entity)
%%%===================================================================
