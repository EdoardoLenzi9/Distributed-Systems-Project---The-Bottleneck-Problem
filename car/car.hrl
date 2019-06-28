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
                        state,
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
                state = Data#carState.state,
                turn = Data#carState.turn,
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
                state = Data#carState.state,
                turn = Data#carState.turn,
                bridgeCapacity =  Data#carState.bridgeCapacity,
                bridgeCrossingTime =  Data#carState.bridgeCrossingTime
            }.

updateState(Data, State) ->
    #carState{  
                name = Data#carState.name,
                side = Data#carState.side,
                power = Data#carState.power,
                delta =  Data#carState.delta,
                arrivalTime = Data#carState.arrivalTime, 
                adj = Data#carState.adj,
                state = State,
                turn = Data#carState.turn,
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