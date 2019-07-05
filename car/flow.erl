%%%===================================================================
%%% spawn support processes
%%%===================================================================
-module(flow).
-compile(export_all).
-include("car.hrl"). 


%%% Spawn a process that launches an event
launch_event(Handler, Args) -> 
    utils:log("launch_event: ~p~p~n", [Handler, Args]),
    spawn(?MODULE, Handler, Args).


%%% Simulate a car crash after a given timeout
timer(Req) ->
    {_Label, Sender, Target, Body} = Req,
    timer:apply_after(Body, car_call_supervisor_api, car_call, [{wait_response, Sender, Target, Body}]).


%%% Change state to NextState, send a Reply to the event sender
next(NextState, Data, From, Reply) ->
    utils:log("STATE TRANSITION -> ~p", [NextState]),
    utils:log("State: ~p", [Data]),
    NewData = Data#car_state{state = NextState},
    car_call_supervisor_api:car_call({next, Data#car_state.name, none, {Data}}),
    {next_state, NextState, NewData, [{reply, From, Reply}]}.
        

%%% Keep the current state, send a Reply to the event sender
keep(Data, From, Reply) ->
    utils:log("KEEP STATE"),
    {keep_state, Data, [{reply, From, Reply}]}.


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
%    Responses = message:send_to_all_adj(Data#car_state.adj#adj.front_cars ++ Data#car_state.adj#adj.rear_cars, check),
%    call_tow_truck_wrap(Responses).
%
%call_tow_truck_wrap([]) ->
%    [];
%call_tow_truck_wrap([{Car, Response} | Rest]) ->
%    if Response =/= ok ->
%        launch_event(tow_truck, [Car#car_state.name, 3000])
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