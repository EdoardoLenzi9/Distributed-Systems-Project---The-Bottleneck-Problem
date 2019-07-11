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
    {timeout, Target} ->  
        utils:log("Event timeout"), 
        car_call_supervisor_api:car_call({  call_tow_truck, 
                                            Data#car_state.name, 
                                            Target, 
                                            Data#car_state.max_RTT, 
                                            Data#car_state.tow_truck_time}),
        flow:keep(Data, From, {sync_timeout, Data});
    {update, Replacement} ->  
        utils:log("Event update"), 
        NewData = Data#car_state{ adj = Data#car_state.adj#adj{front_cars = Replacement} },
        flow:keep(NewData, From, {sync_check, NewData});
    {check, Req} ->  
        utils:log("Event check"), 
        {_Label, Sender, _Target, Nickname, SendingTime, _Body} = Req,
        NewData = Data#car_state{current_time = utils:get_timestamp()},
        car_reply_supervisor_api:car_reply({check_reply, Data#car_state.name, Sender, Nickname, SendingTime, NewData}),
        flow:keep(NewData, From, {sync_check, NewData});
    {crossing, _Req} ->   
        utils:log("Event crossing"), 
        {keep_state, Data, [postpone]};  
    {crash, CrashType} ->   
        utils:log("Event crash"), 
        NewData = Data#car_state{ crash_type = CrashType },
        flow:keep(NewData, From, {sync_crash, NewData});
    {check_reply, Reply} ->
        utils:log("Event check_reply"),
        % berkeley
        {_Sender, _Target, SendingTime, RTT, Body} = Reply,
        CurrentTime = SendingTime, 
        PivotTime = Body#car_state.current_time,
        Delta = CurrentTime - (PivotTime + RTT / 2),
        NewData = Data#car_state{delta = Delta, arrival_time = Data#car_state.arrival_time + Delta},
        car_call_supervisor_api:car_call({adj, NewData#car_state.name, none, NewData#car_state.max_RTT, NewData}),
        flow:keep(NewData, From, {sync_check_reply, NewData});
    {adj_reply, Adj} ->
        utils:log("Event adj_reply"),
        NewData = Data#car_state{adj = Adj},
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
        NewData2 = NewData#car_state{speed = 0, position = Position, current_time = utils:get_timestamp(), synchronized = true},
        flow:next(normal, NewData2, From, {normal, NewData2});
    default_behaviour ->
        utils:log("Event default_behaviour"),
        FrontCars = Data#car_state.adj#adj.front_cars,
        if length(FrontCars) > 0 ->
            [Pivot | _Rest] = FrontCars,
            utils:log("Start call"),
            car_call_supervisor_api:car_call({  check, 
                                                Data#car_state.name, 
                                                Pivot#car_state.name, 
                                                Data#car_state.max_RTT, 
                                                {}
                                            }),
            flow:keep(Data, From, {sync_default_behaviour, Data});
        true ->
            car_call_supervisor_api:car_call({adj, Data#car_state.name, none, Data#car_state.max_RTT, Data})
        end,
        flow:keep(Data, From, {sync_default_behaviour, Data});
    Event ->
        utils:log("Ignore unhandled events (crossing, etc.): ~p", [Event]),
        flow:keep(Data, From, {sync_ignore, Data})
    end.


normal({call, From}, Event, Data) ->
    utils:log("STATE Normal"),
    case Event of        
        {check, Req} ->     
            utils:log("Event check"),
            {_Label, Sender, _Target, SendingTime, _Body} = Req,
            car_reply_supervisor_api:car_reply({check_reply, Data#car_state.name, Sender, SendingTime, Data#car_state{current_time = utils:get_timestamp()}}),
            flow:keep(Data, From, {normal_check, Data});
        {check_reply, Reply} ->
            utils:log("Event check_reply"),
            {_Sender, _Target, _SendingTime, _RTT, Body} = Reply,

            Position = compute_position(Data),

            % if car reahes the bridge go to leader state
            if (Position * Data#car_state.side) =< 0 ->
                utils:log("Car: reach the bridge"),
                NewData = Data#car_state{position = 0, speed = 0},
                flow:next(leader, NewData, From, {leader, NewData});
            % otherwise continue polling
            true ->
                Offset = (Data#car_state.size / 2 + Body#car_state.size / 2),
                Distance = if Body#car_state.crossing ->
                    Data#car_state.position - (Body#car_state.position - (Body#car_state.bridge_length * Body#car_state.side));
                true ->
                        Data#car_state.position - Body#car_state.position
                end,
                Speed = Distance / Data#car_state.max_RTT,

                % continue polling
                FrontCars = Data#car_state.adj#adj.front_cars,
                if length(FrontCars) > 0 ->
                    [Pivot | _Rest] = FrontCars,
                    car_call_supervisor_api:car_call({check, Data#car_state.name, Pivot#car_state.name, Data#car_state.max_RTT, {}})
                end,
                NewData = Data#car_state{position = Position, speed = Speed, current_time = utils:get_timestamp() },
                flow:keep(NewData, From, {normal_check_reply, NewData})
            end;
        {crash, CrashType} ->
            utils:log("Event crash"),
            NewData = Data#car_state{ crash_type = CrashType },
            flow:next(dead, NewData, From, {dead, NewData});
        default_behaviour ->
            utils:log("Event default_behaviour"),

            if Data#car_state.crash_type > 0 ->
                utils:log("Postponed events, car crashed during sync"),
                car_call_supervisor_api:car_call({crash, Data#car_state.name, Data#car_state.name, Data#car_state.max_RTT, Data#car_state.crash_type});
            true ->
                ok
            end,

            Position = compute_position(Data),

            if (Position * Data#car_state.side) =< 0 ->
                if Data#car_state.crossing ->
                    utils:log("Car: reach the end of the bridge"),
                    flow:next(dead, Data, From, {dead, Data});
                true ->
                    % if car reaches the bridge go to leader state
                    utils:log("Car: reach the bridge"),
                    NewData = Data#car_state{position = 0, crossing = true, speed = 0},
                    flow:next(leader, NewData, From, {leader, NewData})
                end;
            true ->
                if Data#car_state.crossing ->
                    utils:log("Car: cross the bridge"),
                    CarCross = Data#car_state{position = Position},    
                    flow:keep(CarCross, From, {normal_check_reply, CarCross});
                true->
                    utils:log("Car: away from the bridge"),
                    FrontCars = Data#car_state.adj#adj.front_cars,
                    NewData = if length(FrontCars) > 0 ->
                                [Pivot | _Rest] = FrontCars,
                                % if there is another car on the same side
                                if Pivot#car_state.side == Data#car_state.side ->
                                    utils:log("~p", [Pivot]),
                                    utils:log("Car: there is another car on the same side"),
                                    % launch a check
                                    car_call_supervisor_api:car_call({check, Data#car_state.name, Pivot#car_state.name, Data#car_state.max_RTT, {}}),
                                    Data;
                                % if there is only a car on the opposite side of the bridge
                                true ->
                                    utils:log("Car: there is only a car on the opposite side of the bridge"),
                                    Speed = erlang:min(((Data#car_state.position * Data#car_state.side) / Data#car_state.max_RTT), Data#car_state.max_speed),
                                    car_call_supervisor_api:car_call({wait, Data#car_state.name, none, Data#car_state.max_RTT, Data#car_state.max_RTT}),
                                    Data#car_state{speed = Speed}
                                end;
                            % if there isn't any other car 
                            true ->
                                utils:log("Car: there isn't any other car in the front queue"),
                                Speed = erlang:min(((Data#car_state.position * Data#car_state.side) / Data#car_state.max_RTT), Data#car_state.max_speed),
                                car_call_supervisor_api:car_call({wait, Data#car_state.name, none, Data#car_state.max_RTT, Data#car_state.max_RTT}),
                                Data#car_state{speed = Speed}
                            end,
                    flow:keep(NewData, From, {normal_default_behaviour, Data})
                end
    end
end.


compute_position(Data) ->
    % compute car position
    TravelTime = utils:get_timestamp() - Data#car_state.current_time,
    Position = Data#car_state.position + (Data#car_state.speed * TravelTime * (-1 * Data#car_state.side)),
    utils:log("Travel Time: ~p, Speed: ~p, OldPosition: ~p, CurrentPosition: ~p", [TravelTime, Data#car_state.speed, Data#car_state.position, Position]),
    Position.


leader({call, From}, Event, Data) ->
    utils:log("STATE Leader"),
    case Event of
        {check, Req} ->     
            utils:log("Event check"),
            {_Label, Sender, _Target, SendingTime, _Body} = Req,
            car_reply_supervisor_api:car_reply({check_reply, Data#car_state.name, Sender, SendingTime, Data#car_state{current_time = utils:get_timestamp()}}),
            flow:keep(Data, From, {leader_check, Data});
        {check_reply, Reply} ->
            utils:log("Event check_reply"),
            {_Label, _Sender, _Target, _SendingTime, _RTT, Body} = Reply, 

            if Body#car_state.arrival_time > Data#car_state.arrival_time ->
                %the car can start crossing the bridge
                Position = Data#car_state.bridge_length, 
                Bridge_capacity = (Data#car_state.bridge_capacity - 1),
                Speed = Data#car_state.max_speed,
                NewData = Data#car_state{
                                        position = Position, 
                                        bridge_capacity = Bridge_capacity,
                                        crossing = true, 
                                        speed = Speed
                                    },
                if length(Data#car_state.adj#adj.rear_cars) > 0 ->
                    RearCars = Data#car_state.adj#adj.rear_cars,
                    [Pivot | _Rest] = RearCars,
                    utils:log("Start call"),
                    car_call_supervisor_api:car_call({check, Data#car_state.name, Pivot#car_state.name, _SendingTime, {cross,NewData, Body}});
                true ->
                    utils:log("Nobody behind, nothing to propagate")
                end,
                flow:next(normal, NewData, From, {normal, NewData});
            true->
                %the car must wait (has arrived after)
                flow:keep(Data, From, default_behaviour)
            end;
        crash ->
            flow:next(dead, Data, From, {dead, Data});
        default_behaviour ->
            FrontCars = Data#car_state.adj#adj.front_cars,
            RearCars = Data#car_state.adj#adj.rear_cars,
            if length(FrontCars) > 0 ->
                [Pivot|_Rest] = FrontCars,
                car_call_supervisor_api:car_call({check, Data#car_state.name, Pivot#car_state.name, {}});
            true ->
                %the car can start crossing the bridge
                Position = Data#car_state.bridge_length, 
                Bridge_capacity = (Data#car_state.bridge_capacity - 1),
                Speed = Data#car_state.max_speed,
                RearCars = Data#car_state.adj#adj.rear_cars,
                NewData = Data#car_state{
                                        position = Position, 
                                        bridge_capacity = Bridge_capacity,
                                        crossing = true, 
                                        speed = Speed
                                    },
                if length(RearCars) > 0 ->
                    [Pivot | _Rest] = RearCars,
                    utils:log("Start call"),
                    car_call_supervisor_api:car_call({check, Data#car_state.name, Pivot#car_state.name, {}});
                true ->
                    utils:log("Nothing to propagate")
                end,
                flow:next(normal, NewData, From, {normal, NewData})
            end,
            flow:keep(Data, From, {leader_default_behaviour, Data})
        end.


dead({call, From}, Event, Data) -> 
    utils:log("STATE Dead"),
    case Event of    
        tow_truck ->
            utils:log("Event tow_truck"),
            notify_dead_and_stop(Data),
            flow:keep(Data, From, {dead_tow_truck, Data});
        default_behaviour ->
            utils:log("Event default_behaviour"),
            case Data#car_state.crash_type of 
                0 -> 
                    notify_dead_and_stop(Data),
                    flow:keep(Data, From, {dead_default_behaviour_0, Data});
                1 -> 
                    car_call_supervisor_api:car_call({  call_tow_truck, 
                                                        Data#car_state.name, 
                                                        Data#car_state.name,  
                                                        Data#car_state.max_RTT, 
                                                        Data#car_state.tow_truck_time}),
                    flow:keep(Data, From, {dead_default_behaviour_1, Data});
                2 -> 
                    flow:keep(Data, From, {dead_default_behaviour_2, Data})
            end;
        _ ->
            {keep_state, Data, [postpone]}  
    end.


notify_dead_and_stop(Data) ->
    FrontCar = if length(Data#car_state.adj#adj.front_cars) > 0 ->
        [First | _Rest] = Data#car_state.adj#adj.front_cars,
        First;
    true -> 
        []
    end,
    RearCar = if length(Data#car_state.adj#adj.rear_cars) > 0 ->
        [Last | _] = lists:reverse(Data#car_state.adj#adj.rear_cars),
        Last;
    true -> 
        []
    end,
    if FrontCar =/= [] ->
        car_call_supervisor_api:car_call({  update_rear, 
                                            Data#car_state.name, 
                                            FrontCar#car_state.name,  
                                            Data#car_state.max_RTT, 
                                            RearCar})
    end,
    if RearCar =/= [] ->
        car_call_supervisor_api:car_call({  update_front, 
                                            Data#car_state.name, 
                                            RearCar#car_state.name,  
                                            Data#car_state.max_RTT, 
                                            FrontCar})
    end,
    car_call_supervisor_api:car_call({  stop, 
                                        Data#car_state.name, 
                                        Data#car_state.name,  
                                        Data#car_state.max_RTT, 
                                        {}}).