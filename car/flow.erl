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
    timer:apply_after(Body, car_call_supervisor_api, car_call, [{wait_reply, Sender, Target, Body}]).


nickname(Name, Index) ->
    Nickname = list_to_atom(string:concat(Name, lists:flatten(io_lib:format("~p", [Index])))),
    case whereis(Nickname) of
        undefined ->
            Nickname;
        _ ->
            nickname(Name, Index + 1)
    end.


%%% Simulate a car crash after a given timeout
request_timer(Req) ->
    utils:log("Timer: receive request"),
    {Label, Sender, Target, CurrentTime, RTT, Body} = Req,
    Nickname = nickname(lists:flatten(io_lib:format("~p", [CurrentTime])), 0),
    utils:log("Timer: find available nickname ~p", [Nickname]),
    register(Nickname, self()),
    utils:log("Timer: send call"),
    supervisor_call_supervisor_api:timer_call({Label, Sender, Target, Nickname, CurrentTime, Body}),
    receive
        {sup_reply, Response} ->
            utils:log("Timer: receive reply"),
            supervisor_reply_supervisor_api:timer_reply(Response)
    after RTT ->
        utils:log("Timer: timeout reached"),
        supervisor_reply_supervisor_api:timer_reply({timeout, Sender, Target, Nickname, CurrentTime, Body})
    end.


%%% Change state to NextState, send a Reply to the event sender
next(NextState, Data, From, Reply) ->
    utils:log("STATE TRANSITION -> ~p", [NextState]),
    utils:log("State: ~p", [Data]),
    NewData = Data#car_state{state = NextState},
    car_call_supervisor_api:car_call({next, Data#car_state.name, none, {NewData}}),
    {next_state, NextState, NewData, [{reply, From, Reply}]}.
        

%%% Keep the current state, send a Reply to the event sender
keep(Data, From, Reply) ->
    utils:log("KEEP STATE ~p", [Data#car_state.state]),
    {keep_state, Data, [{reply, From, Reply}]}.


%%% Keep the current state, and postpone an event
postpone(Data) ->
    utils:log("KEEP STATE ~p, POSTPONE EVENT", [Data#car_state.state]),
    {keep_state, Data, [postpone]} .


%%% Simulate a car crash after a given timeout
killer(Name, Timeout) ->
    timer:apply_after(Timeout, car, crash, [Name]).
    
%%% Simulate a tow truck fix after a given timeout
tow_truck(Name, Timeout) ->
    timer:apply_after(Timeout, car, default_behaviour, [Name]).

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