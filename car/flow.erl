%%%===================================================================
%%% spawn support processes
%%%===================================================================
-module(flow).
-compile(export_all).
-include("car.hrl"). 


%% Spawn a process that launches an event
launchEvent(Handler, Args) -> 
    utils:log("launchEvent: ~p~p~n", [Handler, Args]),
    spawn(?MODULE, Handler, Args).


%% Simulate a car crash after a given timeout
killer(Name, Timeout) ->
    timer:apply_after(Timeout, gen_statem, call, [{global, Name}, crash]).


%% Simulate a car crash after a given timeout
crossingTimer(Name, Timeout) ->
    timer:apply_after(Timeout, gen_statem, call, [{global, Name}, crossed]).


%% Simulate a tow truck fix after a given timeout
towTruck(Name, Timeout) ->
    timer:apply_after(Timeout, ?MODULE, sendEvent, [{global, Name}, defaultBehaviour]).


callTowTruck(Data) ->
    Responses = message:sendToAllAdj(Data#carState.adj#adj.frontCars ++ Data#carState.adj#adj.rearCars, check),
    callTowTruckWrap(Responses).

callTowTruckWrap([]) ->
    [];
callTowTruckWrap([{Car, Response} | Rest]) ->
    if Response =/= ok ->
        launchEvent(towTruck, [Car#carState.name, 3000])
    end,
    callTowTruckWrap(Rest).


%% Launch a given event until success (polling)
launcher(Name, Event) ->
    io:format("loop detection"),
    try gen_statem:call({global, Name}, Event) of 
        _ -> { } 
    catch 
        exit:_ -> {launcher(Name, Event)}; 
        error:_ -> {launcher(Name, Event)};
        throw:_ -> {launcher(Name, Event)} 
    end. 


next(NextState, Data, From, Reply) ->
    utils:log("STATE TRANSITION -> ~p", [NextState]),
    NewData = updateState(Data, NextState),
    %Adj = http_client:getAdj(NewData),
    %{next_state, NextState, updateAdj(NewData, Adj), [{reply, From, Reply}]}.
    {next_state, NextState, NewData, [{reply, From, Reply}]}.
        

keep(Data, From, Reply) ->
    utils:log("KEEP STATE"),
    {keep_state, Data, [{reply, From, Reply}]}.