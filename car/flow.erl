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
    {_Label, Sender, Target, RTT, Body} = Req,
    timer:apply_after(RTT, car_call_supervisor_api, car_call, [{wait_reply, Sender, Target, RTT, Body}]).


nickname(Name, Index) ->
    Nickname = list_to_atom(string:concat(Name, lists:flatten(io_lib:format("~p", [Index])))),
    case whereis(Nickname) of
        undefined ->
            Nickname;
        _ ->
            nickname(Name, Index + 1)
    end.


%%% request timer
request_timer(Req) ->
    utils:log("Timer: receive request"),
    {Label, Sender, Target, CurrentTime, RTT, Body} = Req,
    Nickname = nickname(lists:flatten(io_lib:format("~p", [CurrentTime])), 0),
    utils:log("Timer: find available nickname ~p", [Nickname]),
    register(Nickname, self()),
    utils:log("Timer: send call to ~p", [Target]),
    supervisor_call_supervisor_api:timer_call({Label, Sender, Target, Nickname, CurrentTime, Body}),
    receive
        {sup_reply, Reply} ->
            utils:log("Timer: receive reply"),
            {ReplyLabel, ReplySender, ReplyTarget, _ReplyNickname, ReplySendingTime, ReplyBody} = Reply,
            if Body =/= dead_ignore ->
                case ReplyLabel of 
                    check_reply ->
                        supervisor_reply_supervisor_api:timer_reply({ReplyLabel, ReplySender, ReplyTarget, ReplySendingTime, ReplyBody});
                    crash ->
                        utils:log("Timer for crashing received");
                    _ ->
                        utils:log("Timer receive ~p reset timeout", [ReplyLabel])
                end;
            true ->
                utils:log("Timer: car is dead"),
                supervisor_reply_supervisor_api:timer_reply({timeout, ReplyTarget, ReplySender, ReplySendingTime, ReplyBody})
            end
    after RTT ->
        utils:log("Timer: timeout reached"),
        supervisor_reply_supervisor_api:timer_reply({timeout, Sender, Target, CurrentTime, Body})
    end.


%%% Change state to NextState, send a Reply to the event sender
next(NextState, Data, From, Reply) ->
    utils:log("STATE TRANSITION -> ~p", [NextState]),
    utils:log("State: ~p", [Data]),
    NewData = Data#car_state{state = NextState},
    car_call_supervisor_api:car_call({next, Data#car_state.name, none, Data#car_state.max_RTT, NewData}),
    {next_state, NextState, NewData, [{reply, From, Reply}]}.
        
%%% Keep the current state, send a Reply to the event sender
keep(Data, From, Reply) ->
    utils:log("KEEP STATE ~p", [Data#car_state.state]),
    {keep_state, Data, [{reply, From, Reply}]}.


ignore(State, Event, Data, From) ->
    utils:log("Ignore unhandled event: ~p", [Event]),
    keep(Data, From, {list_to_atom(string:concat(atom_to_list(State),"_ignore")), Data}).


%%% Keep the current state, and postpone an event
postpone(Data) ->
    utils:log("KEEP STATE ~p, POSTPONE EVENT", [Data#car_state.state]),
    {keep_state, Data, [postpone]} .


%%% Simulate a car crash after a given timeout
killer(Name, Data, CrashType, Timeout) ->
    utils:log("KILLER ACTIVATED"),
    %NewData = Data#car_state{crash_type = CrashType},
    timer:apply_after(Timeout, car_call_supervisor_api, car_call, [{crash, Name, none, 0, CrashType}]).
    
%%% Simulate a tow truck fix after a given timeout
tow_truck(Timeout, Data, Target) ->
    timer:apply_after(Timeout, car_call_supervisor_api, car_call, [{tow_truck, Target, Target, Data#car_state.max_RTT, {}}]).
