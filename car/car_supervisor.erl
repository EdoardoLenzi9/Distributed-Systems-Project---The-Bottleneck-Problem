%%%===================================================================
%%% Car supervisor interface
%%%===================================================================

-module(car_supervisor).
-compile(export_all).
-include("car.hrl").

start(Args) -> 
    
    [PName, PSide, PPower, PTurn, PBridgeCapacity, PBridgeLength, PTimeout] = Args,
    {Name, _} = string:to_integer(PName),
    {Side, _} = string:to_integer(PSide),
    {Power, _ } = string:to_integer(PPower),
    {Turn, _ } = string:to_integer(PTurn),
    {BridgeCapacity, _ } = string:to_integer(PBridgeCapacity),
    {BridgeLength, _ } = string:to_integer(PBridgeLength),
    {Timeout, _ } = string:to_integer(PTimeout),
    
    register(Name, self()),    
    Env = utils:load_environment(),

    State = #car_state{
                        name = Name, 
                        side = Side, 
                        power = Power, 
                        adj = #adj{front_cars = http_client:get_sync(Name, Side, Power), rear_cars = []}, 
                        arrival_time = utils:get_timestamp(), 
                        state = init,
                        turn = Turn,
                        bridge_capacity = BridgeCapacity, 
                        bridge_length = BridgeLength,
                        max_speed = Env#env.max_speed,
                        tow_truck_time = Env#env.tow_truck_time
                    },

	if Timeout > 0 ->
        utils:log("Launch killer process with timeout")
        %launch killer
    end,
    car:start_link(State),
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
                    http_client:get_adj(Body);
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
