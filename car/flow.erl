%%%===================================================================
%%% spawn support processes
%%%===================================================================
-module(flow).
-compile(export_all).
-include("car.hrl"). 


%%% Spawn a process that launches an event
%launch_event(Handler, Args) -> 
%    utils:log("launch_event: ~p~p~n", [Handler, Args]),
%    spawn(?MODULE, Handler, Args).
%
%
%%% Simulate a car crash after a given timeout
%killer(Name, Timeout) ->
%    timer:apply_after(Timeout, gen_statem, call, [{global, Name}, crash]).
%
%
%%% Simulate a car crash after a given timeout
%crossing_timer(Name, Timeout) ->
%    timer:apply_after(Timeout, gen_statem, call, [{global, Name}, crossed]).
%
%
%%% Simulate a tow truck fix after a given timeout
%tow_truck(Name, Timeout) ->
%    timer:apply_after(Timeout, ?MODULE, send_event, [{global, Name}, default_behaviour]).
%
%
%call_tow_truck(Data) ->
%    Responses = message:send_to_all_adj(Data#carState.adj#adj.frontCars ++ Data#carState.adj#adj.rearCars, check),
%    call_tow_truck_wrap(Responses).
%
%call_tow_truck_wrap([]) ->
%    [];
%call_tow_truck_wrap([{Car, Response} | Rest]) ->
%    if Response =/= ok ->
%        launch_event(tow_truck, [Car#carState.name, 3000])
%    end,
%    call_tow_truck_wrap(Rest).
%
%
%%% Launch a given event until success (polling)
%launcher(Name, Event) ->
%    io:format("loop detection"),
%    try gen_statem:call({global, Name}, Event) of 
%        _ -> { } 
%    catch 
%        exit:_ -> {launcher(Name, Event)}; 
%        error:_ -> {launcher(Name, Event)};
%        throw:_ -> {launcher(Name, Event)} 
%    end. 


next(NextState, Data, From, Reply) ->
    utils:log("STATE TRANSITION -> ~p", [NextState]),
    NewData = update_state(Data, NextState),
    %Adj = http_client:get_adj(NewData),
    %{next_state, NextState, update_adj(NewData, Adj), [{reply, From, Reply}]}.
    {next_state, NextState, NewData, [{reply, From, Reply}]}.
        

keep(Data, From, Reply) ->
    utils:log("KEEP STATE"),
    {keep_state, Data, [{reply, From, Reply}]}.