%%%===================================================================
%%% macros and record definitions
%%%===================================================================

-record (carState, {    
                        % car metadata
                        name,
                        side,
                        power,
                        speed,
                        position,
                        crossing,
                        delta,
                        sendingTime,
                        arrivalTime, 
                        currentTime,
                        adj,
                        state,
                        % settings and bridge metadata 
                        turn,
                        bridgeCapacity,
                        bridgeLength,
                        maxSpeed,
                        towTruckTime
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


-record (env, {    
                host,
                maxSpeed,
                turn,
                bridgeCapacity,
                bridgeLength,
                towTruckTime
            }).

%%%===================================================================
%%% update records
%%%===================================================================

updateAdj(Data, Adj) ->
    #carState{  
                name = Data#carState.name,
                side = Data#carState.side,
                power = Data#carState.power,
                speed = Data#carState.speed,
                position = Data#carState.position,
                crossing = Data#carState.crossing,
                delta = Data#carState.delta,
                sendingTime = Data#carState.sendingTime, 
                arrivalTime = Data#carState.arrivalTime, 
                currentTime = Data#carState.currentTime, 
                adj = Adj,
                state = Data#carState.state,
                turn = Data#carState.turn,
                bridgeCapacity = Data#carState.bridgeCapacity,
                bridgeLength = Data#carState.bridgeLength,
                maxSpeed = Data#carState.maxSpeed,
                towTruckTime = Data#carState.towTruckTime
            }.


updateDelta(Data, Delta) ->
    #carState{  
                name = Data#carState.name,
                side = Data#carState.side,
                power = Data#carState.power,
                speed = Data#carState.speed,
                position = Data#carState.position,
                crossing = Data#carState.crossing,
                delta = Delta,
                sendingTime = Data#carState.sendingTime, 
                arrivalTime = Data#carState.arrivalTime, 
                currentTime = Data#carState.currentTime, 
                adj = Data#carState.adj,
                state = Data#carState.state,
                turn = Data#carState.turn,
                bridgeCapacity =  Data#carState.bridgeCapacity,
                bridgeLength = Data#carState.bridgeLength,
                maxSpeed = Data#carState.maxSpeed,
                towTruckTime = Data#carState.towTruckTime
            }.

updateState(Data, State) ->
    #carState{  
                name = Data#carState.name,
                side = Data#carState.side,
                power = Data#carState.power,
                speed = Data#carState.speed,
                position = Data#carState.position,
                crossing = Data#carState.crossing,
                delta =  Data#carState.delta,
                sendingTime = Data#carState.sendingTime, 
                arrivalTime = Data#carState.arrivalTime, 
                currentTime = Data#carState.currentTime, 
                adj = Data#carState.adj,
                state = State,
                turn = Data#carState.turn,
                bridgeCapacity =  Data#carState.bridgeCapacity,
                bridgeLength =  Data#carState.bridgeLength,
                maxSpeed = Data#carState.maxSpeed,
                towTruckTime = Data#carState.towTruckTime
            }.



updateSendingTime(Data) ->
    #carState{  
                name = Data#carState.name,
                side = Data#carState.side,
                power = Data#carState.power,
                speed = Data#carState.speed,
                position = Data#carState.position,
                crossing = Data#carState.crossing,
                delta =  Data#carState.delta,
                sendingTime = utils:getTimestamp(), 
                arrivalTime = Data#carState.arrivalTime, 
                currentTime = Data#carState.currentTime, 
                adj = Data#carState.adj,
                state = Data#carState.state,
                turn = Data#carState.turn,
                bridgeCapacity =  Data#carState.bridgeCapacity,
                bridgeLength =  Data#carState.bridgeLength,
                maxSpeed = Data#carState.maxSpeed,
                towTruckTime = Data#carState.towTruckTime
            }.

updateCurrentTime(Data) ->
    #carState{  
                name = Data#carState.name,
                side = Data#carState.side,
                power = Data#carState.power,
                speed = Data#carState.speed,
                position = Data#carState.position,
                crossing = Data#carState.crossing,
                delta =  Data#carState.delta,
                sendingTime = Data#carState.sendingTime, 
                arrivalTime = Data#carState.arrivalTime, 
                currentTime = utils:getTimestamp(), 
                adj = Data#carState.adj,
                state = Data#carState.state,
                turn = Data#carState.turn,
                bridgeCapacity =  Data#carState.bridgeCapacity,
                bridgeLength =  Data#carState.bridgeLength,
                maxSpeed = Data#carState.maxSpeed,
                towTruckTime = Data#carState.towTruckTime
            }.
%%%===================================================================
%%% Unmarshalling mappers (Dto -> Entity)
%%%===================================================================

unmarshalling_sync([]) ->
    [];
unmarshalling_sync([First| Rest]) ->
    { [ {<<"name">>, Name},{<<"side">>,Side},{<<"power">>,Power} ] } = First,
    [#carState{ name = utils:binary_to_atom(Name), 
                side = utils:binary_to_atom(Side), 
                power = Power } | unmarshalling_sync(Rest)].

unmarshalling_adj([ [], [] ]) ->
    #adj{ frontCars = [], rearCars = [] };
unmarshalling_adj([ [Front], [Back] ]) ->
    #adj{ frontCars = unmarshalling_adj_wrapper(Front), rearCars = unmarshalling_adj_wrapper(Back) }.

unmarshalling_adj_wrapper([]) ->
    [];
unmarshalling_adj_wrapper([First| Rest]) ->
    { [ {<<"name">>, Name}, 
        {<<"side">>,Side},
        {<<"power">>,Power},
        {<<"arrivalTime">>,ArrivalTime},
        {<<"delta">>,Delta},
        {<<"state">>,State} ] } = First,
    [#carState{ name = utils:binary_to_atom(Name), 
                side = utils:binary_to_atom(Side), 
                power = Power,
                arrivalTime = ArrivalTime,
                delta = Delta,
                state = utils:binary_to_atom(State)} | unmarshalling_adj_wrapper(Rest)].    