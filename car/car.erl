-module(car).
-behaviour(gen_statem).

-include("car.hrl"). 
-include("car_std.hrl").
-include("car_api.hrl").


%%%===================================================================
%%% gen_statem states
%%%===================================================================


init([State]) ->
    utils:log("STATE Init"),
    {ok, sync, State}.
        

sync({call, From}, Event, Data) ->
    utils:log("STATE Sync"),
    case Event of  
    {check, Req} ->     
        utils:log("Event check"),
        {_Label, Sender, _Target, SendingTime, _Body} = Req,
        car_response_supervisor_api:car_response({check_response, Data#car_state.name, Sender, SendingTime, Data#car_state{current_time = utils:get_timestamp()}}),
        flow:keep(Data, From, {sync_check, Data});
    {response_check, Response} ->
        utils:log("Event response_check"),
        % berkeley
        {_Label, _Sender, _Target, SendingTime, RTT, Body} = Response,
        CurrentTime = SendingTime, 
        PivotTime = Body#car_state.current_time,
        Delta = CurrentTime - (PivotTime + RTT / 2),
        NewData = Data#car_state{delta = Delta, arrival_time = Data#car_state.arrival_time + Delta},
        car_call_supervisor_api:car_call({adj, NewData#car_state.name, none, NewData}),
        flow:keep(NewData, From, {sync_response_check, NewData});
    {response_adj, Response} ->
        utils:log("Event response_adj"),
        {_Label, _Sender, _Target, _SendingTime, _RTT, Body} = Response,
        NewData = Data#car_state{adj = Body},
        utils:log("~p", [NewData]),
        Position = if length(NewData#car_state.adj#adj.front_cars) > 0 ->
            [First | _Rest] = NewData#car_state.adj#adj.front_cars,
            % if there is any car in the front queue and on the same side
            if First#car_state.side == NewData#car_state.side ->
                First#car_state.position + First#car_state.side; 
            % if there are only cars on the opposite side
            true -> 
                Data#car_state.side
            end;
        % if there isn't any other car in the front queue
        true ->
            Data#car_state.side
        end,
        NewData2 = NewData#car_state{speed = 0, position = Position, current_time = utils:get_timestamp()},
        flow:next(normal, NewData2, From, {normal, NewData2});
    default_behaviour ->
        utils:log("Event default_behaviour"),
        FrontCars = Data#car_state.adj#adj.front_cars,
        if length(FrontCars) > 0 ->
            [Pivot | _Rest] = FrontCars,
            utils:log("Start call"),
            car_call_supervisor_api:car_call({check, Data#car_state.name, Pivot#car_state.name, {}}),
            flow:keep(Data, From, {sync_default_behaviour, Data});
        true ->
            car_call_supervisor_api:car_call({adj, Data#car_state.name, none, Data})
        end,
        flow:keep(Data#car_state{delta = 0}, From, {sync_default_behaviour, Data#car_state{delta = 0}})
    end.


normal({call, From}, Event, Data) ->
    utils:log("STATE Normal"),
    case Event of        
        {check, Req} ->     
            utils:log("Event check"),
            {_Label, Sender, _Target, SendingTime, _Body} = Req,
            car_response_supervisor_api:car_response({check_response, Data#car_state.name, Sender, SendingTime, Data#car_state{current_time = utils:get_timestamp()}}),
            flow:keep(Data, From, {normal_check, Data});
        {response_check, Response} ->
            utils:log("Event response_check"),
            {_Label, _Sender, _Target, _SendingTime, _RTT, Body} = Response,

            Position = compute_position(Data),

            Distance = Data#car_state.position - Body#car_state.position,
            FrontCars = Data#car_state.adj#adj.front_cars,
            if length(FrontCars) > 0 ->
                [Pivot | _Rest] = FrontCars,
                car_call_supervisor_api:car_call({check, Data#car_state.name, Pivot#car_state.name, {}})
            end,
            flow:keep(Data, From, {normal_response_check, Data});
        default_behaviour ->
            utils:log("Event default_behaviour"),

            Position = compute_position(Data),

            % if car reaches the bridge go to leader state
            if (Position * Data#car_state.side) =< 0 ->
                utils:log("Car reaches the bridge"),
                NewData = Data#car_state{position = 0, speed = 0},
                flow:next(leader, NewData, From, {leader, NewData});
            true ->
                utils:log("Car away from the bridge"),
                FrontCars = Data#car_state.adj#adj.front_cars,
                NewData = if length(FrontCars) > 0 ->
                    [Pivot | _Rest] = FrontCars,
                    % if there is another car on the same side
                    if Pivot#car_state.side == Data#car_state.side ->
                        utils:log("there is another car on the same side"),
                        % launch a check
                        car_call_supervisor_api:car_call({check, Data#car_state.name, Pivot#car_state.name, {}}),
                        Data;
                    % if there is only a car on the opposite side of the bridge
                    true ->
                        utils:log("there is only a car on the opposite side of the bridge"),
                        Speed = erlang:min(((Data#car_state.position * Data#car_state.side) / Data#car_state.turn), Data#car_state.max_speed),
                        car_call_supervisor_api:car_call({wait, Data#car_state.name, none, Data#car_state.turn}),
                        Data#car_state{speed = Speed}
                    end;
                % if there isn't any other car 
                true ->
                    utils:log("there isn't any other car in the front queue"),
                    Speed = erlang:min(((Data#car_state.position * Data#car_state.side) / Data#car_state.turn), Data#car_state.max_speed),
                    car_call_supervisor_api:car_call({wait, Data#car_state.name, none, Data#car_state.turn}),
                    Data#car_state{speed = Speed}
                end,
                flow:keep(NewData, From, {normal_default_behaviour, Data})
            end
    end.


compute_position(Data) ->
    % compute car position
    TravelTime = utils:get_timestamp() - Data#car_state.current_time,
    Position = Data#car_state.position + (Data#car_state.speed * TravelTime * (-1 * Data#car_state.side)),
    utils:log("Travel Time: ~p, Speed: ~p, OldPosition: ~p, CurrentPosition: ~p", [TravelTime, Data#car_state.speed, Data#car_state.position, Position]),
    Position.


leader({call, From}, Event, Data) ->
    utils:log("STATE Queue"),
    case Event of
        default_behaviour ->
            flow:keep(From, Data)
        end.


dead({call, _From}, Event, Data) -> 
    utils:log("STATE Dead"),
    case Event of
        default_behaviour ->
            utils:log("Event default_behaviour"),
            stop(Data#car_state.name)
    end.