%%%===================================================================
%%% Car supervisor interface
%%%===================================================================

-module(car_supervisor).
-compile(export_all).
-include("car.hrl").


start(Args) -> 
    utils:log("Args: ~p", [Args]),
    [PName, PSide, PPower, PSize, PBridgeCapacity, PBridgeLength, PMaxSpeed, PTowTruckTime, PMaxRTT, PTimeout] = Args,
    Name = list_to_atom(PName),
    {Side, _} = string:to_integer(PSide),
    {Power, _ } = string:to_integer(PPower),
    {Size, _ } = string:to_integer(PSize),
    {BridgeCapacity, _ } = string:to_integer(PBridgeCapacity),
    {BridgeLength, _ } = string:to_integer(PBridgeLength),
    {MaxSpeed, _ } = string:to_integer(PMaxSpeed),
    {TowTruckTime, _ } = string:to_integer(PTowTruckTime),
    {MaxRTT, _ } = string:to_integer(PMaxRTT),
    {Timeout, _ } = string:to_integer(PTimeout),
    
    register(supervisor, self()),    
    Env = utils:load_environment(),

    State = #car_state{
                        name = Name, 
                        side = Side, 
                        power = Power, 
                        size = Size,
                        speed = 0,
                        crossing = false,
                        synchronized = false,
                        crash_type = 0,
                        delta = 0,
                        adj = #adj{front_cars = http_client:get_sync(Name, Side, Power), rear_cars = []}, 
                        state = init,
                        host = Env#env.host,
                        bridge_capacity = BridgeCapacity, 
                        bridge_length = BridgeLength,
                        max_speed = MaxSpeed,
                        tow_truck_time = TowTruckTime,
                        max_RTT = MaxRTT 
                    },
    utils:log("Timeout: ~p", [Timeout]),
	if Timeout > 0 ->
        utils:log("Launch killer process with timeout"),
        flow:launch_event(killer, [State#car_state.name, Timeout]);
    true ->
        ok
    end,
    car:start_link(State#car_state.name, State),
    car:default_behaviour(State#car_state.name),
    loop().


loop() ->
    receive
        {car_call, Req} ->
            utils:log("Supervisor: receive car_call ~p ", [Req]),
            {ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody} = Req,
            CurrentTime = utils:get_timestamp(),
            case ReqLabel of 
                adj ->
                    Adj = http_client:get_adj(ReqBody),
                    car:adj_reply(ReqSender, Adj);
                next ->
                    car:default_behaviour(ReqSender);
                wait ->
                    flow:launch_event(timer, [Req]);
                wait_reply ->
                    car:default_behaviour(ReqSender);
                default_behaviour ->
                    car:default_behaviour(ReqSender);
                call_tow_truck ->
                    flow:launch_event(tow_truck, [ReqBody, ReqTarget]);
                tow_truck -> 
                    car:tow_truck(ReqTarget);
                stop -> 
                    car:stop(ReqTarget);
                % wild-card used for: check, crossing, update_rear, update_front
                _ ->
                    flow:launch_event(request_timer, [{ReqLabel, ReqSender, ReqTarget, CurrentTime, ReqRTT, ReqBody}])
            end;
        {timer_call, Req} ->
            utils:log("Supervisor: receive timer_call ~p ", [Req]),
            {ReqLabel, ReqSender, ReqTarget, ReqNickname, ReqSendingTime, ReqBody} = Req,
            case ReqLabel of 
                check ->
                    {_Result, Data} = car:check(ReqTarget),
                    supervisor_reply_supervisor_api:sup_reply({check_reply, ReqTarget, ReqSender, ReqNickname, ReqSendingTime, Data});
                crossing ->
                    {Result, _Data} = car:crossing(Req),
                    supervisor_reply_supervisor_api:sup_reply({crossing_reply, ReqTarget, ReqSender, ReqNickname, ReqSendingTime, Result});
                update_rear ->
                    {Result, _Data} = car:update_rear(ReqSender, ReqBody),
                    supervisor_reply_supervisor_api:sup_reply({update_rear_reply, ReqTarget, ReqSender, ReqNickname, ReqSendingTime, Result});
                update_front ->
                    {Result, _Data} = car:update_front(ReqSender, ReqBody),
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
