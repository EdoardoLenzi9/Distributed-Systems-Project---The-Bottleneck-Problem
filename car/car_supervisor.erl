%%%===================================================================
%%% Car supervisor interface
%%%===================================================================

-module(car_supervisor).
-compile(export_all).
-include("car.hrl").


start(Args) -> 
    utils:log("Args: PHost, PPort, PSide, PPower, PSize, PBridgeCapacity, PBridgeLength, PMaxSpeed, PTowTruckTime, PMaxRTT, PCrashType, PTimeout"),
    utils:log("Args: ~p", [Args]),
    [Host, Port, PSide, PPower, PSize, PBridgeCapacity, PBridgeLength, PMaxSpeed, PTowTruckTime, PMaxRTT, PCrashType, PTimeout] = Args,
    {Side, _} = string:to_integer(PSide),
    {Power, _ } = string:to_integer(PPower),
    {Size, _ } = string:to_integer(PSize),
    {BridgeCapacity, _ } = string:to_integer(PBridgeCapacity),
    {BridgeLength, _ } = string:to_integer(PBridgeLength),
    {MaxSpeed, _ } = string:to_integer(PMaxSpeed),
    {TowTruckTime, _ } = string:to_integer(PTowTruckTime),
    {MaxRTT, _ } = string:to_integer(PMaxRTT),
    {CrashType, _} = string:to_integer(PCrashType),
    {Timeout, _ } = string:to_integer(PTimeout),
    
    register(supervisor, self()),    

    State = #car_state{
                        name = node(), 
                        side = Side - 1, 
                        power = Power, 
                        size = Size,
                        speed = 0,
                        obstacle_position = Size / 2 * (Side - 1), 
                        position = Side - 1, 
                        crossing = false,
                        synchronized = false,
                        crash_type = 0,
                        delta = 0,
                        adj = #adj{front_cars = http_client:get_sync(node(), Side - 1, Power, Host, Port), rear_cars = []}, 
                        state = sync,
                        host = Host,
                        bridge_capacity = BridgeCapacity, 
                        bridge_length = BridgeLength,
                        max_speed = MaxSpeed,
                        tow_truck_time = TowTruckTime,
                        max_RTT = MaxRTT,
                        port = Port,
                        last_RTT = 0
                    },
    utils:log("Car adj ~p", [State#car_state.adj]),
    utils:log("Timeout: ~p", [Timeout]),
	if Timeout > 0 ->
        utils:log("Launch killer process with timeout"),
        flow:launch_event(killer, [State#car_state.name, State, CrashType, Timeout]);
    true ->
        ok
    end,
    car:start_link(State#car_state.name, State),
    car:default_behaviour(State#car_state.name),
    loop().


loop() ->
    receive
        smoke_test -> 
            utils:log("Supervisor receives smoke test");
        {car_call, Req} ->
            utils:log("Supervisor: receive car_call ~p ", [Req]),
            {ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody} = Req,
            CurrentTime = utils:get_timestamp(),
            case ReqLabel of 
                adj ->
                    Adj = http_client:get_adj(ReqBody),
                    car:adj_reply(ReqSender, Adj);
                last_adj ->
                    Last = http_client:get_last_adj(ReqBody),
                    car:last_adj_reply(ReqSender, Last);
                log_state -> 
                    http_client:get_adj(ReqBody);
                next ->
                    http_client:get_adj(ReqBody),
                    car:default_behaviour(ReqSender);
                wait ->
                    flow:launch_event(timer, [Req]);
                wait_reply ->
                    car:default_behaviour(ReqSender);
                default_behaviour ->
                    car:default_behaviour(ReqSender);
                call_tow_truck ->
                    utils:log("TOW TRUCK CALLED"),
                    car:tow_truck(ReqSender);
                tow_truck -> 
                    car:tow_truck(ReqTarget);
                stop -> 
                    http_client:get_adj(ReqBody),
                    car:stop(ReqTarget),
                    utils:log("Stop supervisor"),
                    init:stop();
                crash ->
                    car:crash(ReqSender, ReqBody);
                % wild-card used for: check, one_way_check, crossing, update_rear, update_front
                _ ->
                    flow:launch_event(request_timer, [{ReqLabel, ReqSender, ReqTarget, CurrentTime, ReqRTT, ReqBody}])
            end;
        {timer_call, Req} ->
            utils:log("Supervisor: receive timer_call ~p ", [Req]),
            {ReqLabel, ReqSender, ReqTarget, ReqNickname, ReqSendingTime, ReqBody} = Req,
            case ReqLabel of 
                check ->
                    {_Result, Data} = car:check(ReqTarget, ReqBody),
                    supervisor_reply_supervisor_api:sup_reply({check_reply, ReqTarget, ReqSender, ReqNickname, ReqSendingTime, Data});
                one_way_check ->
                    {Result, _Data} = car:check(ReqTarget, ReqBody),
                    supervisor_reply_supervisor_api:sup_reply({one_way_check_reply, ReqTarget, ReqSender, ReqNickname, ReqSendingTime, Result});
                crossing ->
                    {Result, _Data} = car:crossing(Req),
                    supervisor_reply_supervisor_api:sup_reply({crossing_reply, ReqTarget, ReqSender, ReqNickname, ReqSendingTime, Result});
                update_rear ->
                    {Result, _Data} = car:update_rear(ReqTarget, ReqBody),
                    supervisor_reply_supervisor_api:sup_reply({update_rear_reply, ReqTarget, ReqSender, ReqNickname, ReqSendingTime, Result});
                update_front ->
                    {Result, _Data} = car:update_front(ReqTarget, ReqBody),
                    supervisor_reply_supervisor_api:sup_reply({update_front_reply, ReqTarget, ReqSender, ReqNickname, ReqSendingTime, Result})
            end;
        {timer_reply, Reply} ->
            utils:log("Supervisor: receive timer_reply ~p ", [Reply]),
            {ReplyLabel, ReplySender, ReplyTarget, ReplySendingTime, ReplyBody} = Reply,
            RTT = utils:get_timestamp() - ReplySendingTime,
            case ReplyLabel of 
                check_reply ->
                    car:check_reply({ReplySender, ReplyTarget, ReplySendingTime, RTT, ReplyBody});
                timeout ->
                    car:timeout(ReplySender, ReplyTarget)
            end;   
        Any ->
            utils:log("Test: receive unhandled call ~p ", [Any])
    end,
    loop().
