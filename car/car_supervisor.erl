%%%===================================================================
%%% Car supervisor interface
%%%===================================================================

-module(car_supervisor).
-compile(export_all).
-include("car.hrl").


start(Args) -> 
    [Name, PSide, PPower, PBridgeCapacity, PBridgeLength, PMaxSpeed, PTowTruckTime, PMaxRTT, PTimeout] = Args,
    {Side, _} = string:to_integer(PSide),
    {Power, _ } = string:to_integer(PPower),
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
                        speed = 0,
                        crossing = false,
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

	if Timeout > 0 ->
        utils:log("Launch killer process with timeout"),
        flow:killer(State#car_state.name, Timeout)
       
    end,
    car:start_link(State),
    car:default_behaviour(State#car_state.name),
    loop().


loop() ->
    receive
        % Car -> Supervisor call API
        {car_call, Req} ->
            utils:log("Car call supervisor - receive"),
            CurrentTime = utils:get_timestamp(),
            % start timer
            {Label, Sender, Target, Body} = Req,
            case Label of 
                adj ->
                    Adj = http_client:get_adj(Body),
                    car:adj_response({Label, Sender, Target, CurrentTime, 0, Adj});
                _ ->
                    supervisor_call_supervisor_api:sup_call({Label, Sender, Target, CurrentTime, Body})
            end;


        % Supervisor -> Supervisor call API
        {sup_call, Req} ->
            {Label, _Sender, _Target, _SendingTime, _Body} = Req,
            case Label of 
                check ->
                    utils:log("Supervisor call car"),
                    car:check(Req)
            end;


        % Car -> Supervisor response API
        {car_response, Response} ->
            utils:log("Car response - receive"),
            supervisor_call_supervisor_api:sup_response(Response);


        % Supervisor -> Supervisor response API
        {sup_response, Response} ->
            utils:log("Sepervisor response - receive"),
            {Label, Sender, Target, SendingTime, Body} = Response,
            RTT = utils:get_timestamp() - SendingTime,
            car:check_response({Label, Sender, Target, SendingTime, RTT, Body})
    end,
    loop().
