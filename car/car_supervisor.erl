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
    
    register(Name, self()),    
    Env = utils:load_environment(),

    State = #car_state{
                        name = Name, 
                        side = Side, 
                        power = Power, 
                        size = Size,
                        speed = 0,
                        crossing = false,
                        synchronized = false,
                        crashed = false,
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
        flow:killer(State#car_state.name, Timeout)
    end,
    car:start_link(State#car_state.name, State),
    car:default_behaviour(State#car_state.name),
    loop().


loop() ->
    receive
        % Car -> Supervisor call API
        {car_call, Req} ->
            CurrentTime = utils:get_timestamp(),
            % start timer
            {Label, Sender, Target, Body} = Req,
            utils:log("Supervisor receives a car call ~p", [Label]),
            case Label of 
                adj ->
                    utils:log("Supervisor call adj endpoint"),
                    Adj = http_client:get_adj(Body),
                    car:adj_reply({Label, Sender, Target, CurrentTime, 0, Adj});
                next ->
                    car:default_behaviour(Sender);
                wait ->
                    flow:launch_event(timer, [Req]);
                wait_reply ->
                    car:default_behaviour(Sender);
                % wild-card used for: check, crossing
                _ ->
                    flow:launch_event(request_timer, [{Label, Sender, Target, CurrentTime, Body}])
            end;


        % Supervisor -> Supervisor call API
        {sup_call, Req} ->
            {Label, _Sender, _Target, _SendingTime, _Body} = Req,
            utils:log("Supervisor receives a supervisor call ~p", [Label]),
            case Label of 
                check ->
                    car:check(Req);
                crossing ->
                    car:crossing(Req)
            end;


        % Car -> Supervisor reply API
        {car_reply, Response} ->
            {Label, _Sender, _Target, _SendingTime, _Body} = Response,
            utils:log("Supervisor receives a car reply ~p", [Label]),
            supervisor_call_supervisor_api:sup_reply(Response);


        % Supervisor -> Supervisor reply API
        {sup_reply, Response} ->
            {Label, Sender, Target, SendingTime, Body} = Response,
            utils:log("Supervisor receives a supervisor reply ~p", [Label]),
            RTT = utils:get_timestamp() - SendingTime,
            case Label of 
                check ->
                    car:check_reply({Label, Sender, Target, SendingTime, RTT, Body});
                crossing ->
                    car:crossing_reply({Label, Sender, Target, SendingTime, RTT, Body})
            end
    end,
    loop().
