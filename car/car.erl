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
    utils:log("Initial max_speed: ~p", [State#car_state.max_speed]),
    NewState = State#car_state{ arrival_time = utils:get_timestamp(), current_time = utils:get_timestamp()}, 
    {ok, sync, NewState}.
        

sync({call, From}, Event, Data) ->
    utils:log("STATE Sync"),
    case Event of  
    {crash, CrashType} ->   
        utils:log("EVENT crash (postpone)"), 
        NewData = Data#car_state{ crash_type = CrashType },
        flow:keep(NewData, From, {sync_crash, NewData});
    {timeout, Target} ->  
        timeout(sync, Target, Data, From);
    {update_front, Replacement} ->  
        update_front(sync, Replacement, Data, From);
    {update_rear, Replacement} ->  
        update_rear(sync, Replacement, Data, From);
    {check, Sender} ->  
        check(sync, Sender, Data, From);
    {crossing, Body} ->   
        crossing(sync, Body, Data, From);
    {check_reply, Reply} ->
        utils:log("EVENT check_reply"),
        % berkeley
        {_Sender, _Target, SendingTime, RTT, Body} = Reply,
        if Data#car_state.delta == 0 ->
            utils:log("RTT ~p", [RTT]),
            CurrentTime = SendingTime, 
            PivotTime = Body#car_state.current_time,
            Delta = CurrentTime - (PivotTime + RTT / 2),
            NewData = Data#car_state{delta = Delta, arrival_time = Data#car_state.arrival_time + Delta},
            car_call_supervisor_api:car_call({adj, NewData#car_state.name, none, NewData#car_state.max_RTT, NewData}),
            flow:keep(NewData, From, {sync_check_reply, NewData});
        true ->
            [Rear | _Rest] = Body#car_state.adj#adj.rear_cars,
            if Rear#car_state.name == Data#car_state.name ->
                utils:log("Car: No race conflict during sync"),
                Position = Body#car_state.position + (((Data#car_state.size / 2) + (Body#car_state.size / 2)) * Data#car_state.side), 
                utils:log("initial position: ~p", [Position]),
                NewData = Data#car_state{speed = 0, position = Position, current_time = utils:get_timestamp(), synchronized = true},
                flow:next(normal, NewData, From, {normal, NewData});
            true ->
                utils:log("Car: Looses race during sync"),
                NewData = Data#car_state{adj = Data#car_state.adj#adj{front_cars = [Rear]}},
                car_call_supervisor_api:car_call({adj, Data#car_state.name, none, Data#car_state.max_RTT, Data}),
                flow:keep(NewData, From, {sync_check_reply, NewData})
            end
        end;
    {adj_reply, Adj} ->
        utils:log("EVENT adj_reply"),
        NewData = Data#car_state{adj = Adj},
        utils:log("~p", [NewData]),
        if length(NewData#car_state.adj#adj.front_cars) > 0 ->
            [First | _Rest] = NewData#car_state.adj#adj.front_cars,
            utils:log("Macchina davanti: ~p ~p", [First, First#car_state.position]),
            car_call_supervisor_api:car_call({  check, 
                                                Data#car_state.name, 
                                                First#car_state.name, 
                                                Data#car_state.max_RTT, 
                                                Data
                                            }),
            flow:keep(Data, From, {sync_adj_reply, Data});
        true -> 
            Position = NewData#car_state.side * NewData#car_state.size, 
            utils:log("initial position: ~p", [Position]),
            NewData2 = NewData#car_state{speed = 0, position = Position, current_time = utils:get_timestamp(), synchronized = true},
            flow:next(normal, NewData2, From, {normal, NewData2})
        end;
    default_behaviour ->
        utils:log("EVENT default_behaviour"),
        FrontCars = Data#car_state.adj#adj.front_cars,
        if length(FrontCars) > 0 ->
            [Pivot | _Rest] = FrontCars,
            utils:log("Start call"),
            car_call_supervisor_api:car_call({  check, 
                                                Data#car_state.name, 
                                                Pivot#car_state.name, 
                                                Data#car_state.max_RTT, 
                                                Data
                                            }),
            flow:keep(Data, From, {sync_default_behaviour, Data});
        true ->
            car_call_supervisor_api:car_call({adj, Data#car_state.name, none, Data#car_state.max_RTT, Data})
        end,
        flow:keep(Data, From, {sync_default_behaviour, Data});
    Event ->
        flow:ignore(sync, Event, Data, From)
    end.


normal({call, From}, Event, Data) ->
    utils:log("STATE Normal"),
    case Event of   
        {timeout, Target} ->  
            timeout(normal, Target, Data, From);
        {crash, CrashType} ->
            NewData = Data#car_state{crash_type = CrashType},
            flow:next(dead, NewData, From, {dead, NewData});
        {update_front, Replacement} ->  
            update_front(sync, Replacement, Data, From);
        {update_rear, Replacement} ->  
            update_rear(sync, Replacement, Data, From);
        {crossing, Body} ->   
            crossing(normal, Body, Data, From);    
        {check, Sender} ->  
            check(normal, Sender, Data, From);
        {check_reply, Reply} ->
            utils:log("EVENT check_reply"),
            {_Sender, _Target, _SendingTime, _RTT, Body} = Reply,

            Position = compute_position(Data),

            if (Position * Data#car_state.side) =< (Data#car_state.size / 2) ->
                if Data#car_state.crossing ->
                    utils:log("Car: reach the end of the bridge"),
                    NewData = Data#car_state{position = ( Data#car_state.size * Data#car_state.side / 2 ), speed = 0, current_time = utils:get_timestamp()},
                    flow:next(dead, NewData, From, {dead, NewData});
                true ->
                    utils:log("Car: reach the bridge"),
                    NewData = Data#car_state{position = ( (Data#car_state.bridge_length + (Data#car_state.size / 2)) * Data#car_state.side ), speed = 0, crossing = true, current_time = utils:get_timestamp()},
                    flow:next(leader, NewData, From, {leader, NewData})
                end;
            true ->
                utils:log("Car: away from the bridge"),
                Offset = ((Data#car_state.size / 2) + (Body#car_state.size / 2)) * Data#car_state.side,
                MyPosition = if Data#car_state.crossing ->
                    Data#car_state.position + (Data#car_state.bridge_length * Data#car_state.side);
                true ->
                    Data#car_state.position
                end,
                YourPosition = if Body#car_state.crossing ->
                    Body#car_state.position + (Body#car_state.bridge_length * Body#car_state.side);
                true ->
                    Body#car_state.position
                end,
                utils:log("~p MyPosition: ~p", [Data#car_state.position, MyPosition]),
                utils:log("~p YourPosition: ~p", [Body#car_state.position, YourPosition]),
                Distance = (MyPosition - YourPosition - Offset) * Data#car_state.side,
                Speed = Distance  / (Data#car_state.max_RTT / 1000),
                
                utils:log("Car: travel with position ~p, speed ~p, distance ~p, offset ~p", [Position, Speed, Distance, Offset]),
                
                % continue polling
            
                NewData = Data#car_state{position = Position, speed = Speed, current_time = utils:get_timestamp() },
                car_call_supervisor_api:car_call({log_state, NewData#car_state.name, none, NewData#car_state.max_RTT, NewData}),
                car_call_supervisor_api:car_call({wait, Data#car_state.name, none, Data#car_state.max_RTT, Data#car_state.max_RTT}),
                flow:keep(NewData, From, {normal_check_reply, NewData})
            end;
        default_behaviour ->
            utils:log("EVENT default_behaviour"),

            if Data#car_state.crash_type > 0 ->
                utils:log("Postponed events, car crashed during sync"),
                car_call_supervisor_api:car_call({crash, Data#car_state.name, Data#car_state.name, Data#car_state.max_RTT, Data#car_state.crash_type});
            true ->
                ok
            end,

            Position = compute_position(Data),
            if (Position * Data#car_state.side) =< (Data#car_state.size / 2) ->
                if Data#car_state.crossing ->
                    utils:log("Car: reach the end of the bridge"),
                    NewData = Data#car_state{position = ( Data#car_state.size * Data#car_state.side / 2 ), speed = 0, current_time = utils:get_timestamp()},
                    flow:next(dead, NewData, From, {dead, NewData});
                true ->
                    utils:log("Car: reach the bridge"),
                    NewData = Data#car_state{position = ( (Data#car_state.bridge_length + (Data#car_state.size / 2)) * Data#car_state.side ), speed = 0, crossing = true, current_time = utils:get_timestamp()},
                    flow:next(leader, NewData, From, {leader, NewData})
                end;
            true ->
                utils:log("Car: running"),
                FrontCars = Data#car_state.adj#adj.front_cars,
                if length(FrontCars) > 0 ->
                    [FrontCar | _Rest] = FrontCars,
                    if FrontCar#car_state.side == Data#car_state.side ->
                        utils:log("Car: there is another car on the same side"),
                        NewData = Data#car_state{position = Position},
                        car_call_supervisor_api:car_call({log_state, NewData#car_state.name, none, NewData#car_state.max_RTT, NewData}),
                        car_call_supervisor_api:car_call({check, NewData#car_state.name, FrontCar#car_state.name, NewData#car_state.max_RTT, NewData}),
                        flow:keep(NewData, From, {normal_default_behaviour, NewData});
                    true ->
                        utils:log("Car: there is only a car on the opposite side of the bridge"),
                        Speed = erlang:min((((Data#car_state.position * Data#car_state.side) - (Data#car_state.size / 2)) / (Data#car_state.max_RTT / 1000)), Data#car_state.max_speed),
                        utils:log("New speed ~p", [Speed]),
                        NewData = Data#car_state{speed = Speed, position = Position, current_time = utils:get_timestamp()},
                        car_call_supervisor_api:car_call({log_state, NewData#car_state.name, none, NewData#car_state.max_RTT, NewData}),
                        car_call_supervisor_api:car_call({wait, NewData#car_state.name, none, NewData#car_state.max_RTT, NewData#car_state.max_RTT}),
                        flow:keep(NewData, From, {normal_default_behaviour, NewData})
                    end;
                true ->
                    utils:log("Car: there is not any other car in the front queue"),
                    Speed = erlang:min((((Data#car_state.position * Data#car_state.side) - (Data#car_state.size / 2)) / (Data#car_state.max_RTT / 1000)), Data#car_state.max_speed),
                    utils:log("New speed2 ~p", [Speed]),
                    NewData = Data#car_state{speed = Speed, position = Position, current_time = utils:get_timestamp()},
                    car_call_supervisor_api:car_call({log_state, NewData#car_state.name, none, NewData#car_state.max_RTT, NewData}),
                    car_call_supervisor_api:car_call({wait, NewData#car_state.name, none, NewData#car_state.max_RTT, NewData#car_state.max_RTT}),
                    flow:keep(NewData, From, {normal_default_behaviour, NewData})
                end
            end;
        Event ->
            flow:ignore(normal, Event, Data, From)
    end.


leader({call, From}, Event, Data) ->
    utils:log("STATE Leader"),
    case Event of
        {crash, CrashType} ->
            NewData = Data#car_state{crash_type = CrashType},
            flow:next(dead, NewData, From, {dead, NewData});
        {timeout, Target} ->  
            timeout(leader, Target, Data, From);
        {crossing, Body} ->   
            crossing(leader, Body, Data, From);
        {update_front, Replacement} ->  
            update_front(sync, Replacement, Data, From);
        {update_rear, Replacement} ->  
            update_rear(sync, Replacement, Data, From);
        {check, Sender} ->  
            check(leader, Sender, Data, From);
        {check_reply, Reply} ->
            utils:log("EVENT check_reply"),
            {_Sender, _Target, _SendingTime, _RTT, Body} = Reply, 
            if Body#car_state.arrival_time > Data#car_state.arrival_time ->
                %the car can start crossing the bridge
                propagate_crossing(Data, Body),
                flow:next(normal, Data, From, {normal, Data});
            true->
                %the car must wait (has arrived after)
                flow:keep(Data, From, default_behaviour)
            end;
        default_behaviour ->
            FrontCars = Data#car_state.adj#adj.front_cars,
            if length(FrontCars) > 0 ->
                utils:log("There is a car on the opposite side of the bridge"),
                [Pivot|_Rest] = FrontCars,
                car_call_supervisor_api:car_call({check, Data#car_state.name, Pivot#car_state.name, Data#car_state.max_RTT, Data}),
                flow:keep(Data, From, {leader_default_behaviour, Data});
            true ->
                utils:log("the car can start crossing the bridge"),
                utils:log("max speed ~p", [Data#car_state.max_speed]),
                NewData = Data#car_state{   
                                            position = Data#car_state.bridge_length * Data#car_state.side, 
                                            crossing = true, 
                                            speed = Data#car_state.max_speed
                                        },
                utils:log("TODO 1"),
                propagate_crossing(Data, #car_state{arrival_time = -1, bridge_capacity = Data#car_state.bridge_capacity}),
                utils:log("TODO 2"),
                flow:next(normal, NewData, From, {normal, NewData})
            end;
        Event ->
            flow:ignore(leader, Event, Data, From)
    end.


dead({call, From}, Event, Data) -> 
    utils:log("STATE Dead"),
    utils:log("Crash type: ~p", [Data#car_state.crash_type]),
    case Event of    
        tow_truck ->
            utils:log("EVENT tow_truck"),
            notify_dead_and_stop(Data),
            flow:keep(Data, From, {dead_tow_truck, Data});
        default_behaviour ->
            utils:log("EVENT Dead_default_behaviour"),
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
        Event ->
            flow:ignore(dead, Event, Data, From)
    end.


%%%===================================================================
%%% Utility functions
%%%===================================================================


notify_dead_and_stop(Data) ->
    utils:log("Notify dead ~p ~n", [Data#car_state.adj]),
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
        utils:log("Send update rear to front car ~p with body ~p ~n", [FrontCar#car_state.name, RearCar]),
        car_call_supervisor_api:car_call({  update_rear, 
                                            Data#car_state.name, 
                                            FrontCar#car_state.name,  
                                            Data#car_state.max_RTT, 
                                            RearCar});
    true ->
        ok
    end,
    if RearCar =/= [] ->
        utils:log("Send update front to rear car ~p with body ~p ~n", [RearCar#car_state.name, FrontCar]),
        car_call_supervisor_api:car_call({  update_front, 
                                            Data#car_state.name, 
                                            RearCar#car_state.name,  
                                            Data#car_state.max_RTT, 
                                            FrontCar});
    true ->
        ok
    end,
    car_call_supervisor_api:car_call({  stop, 
                                        Data#car_state.name, 
                                        Data#car_state.name,  
                                        Data#car_state.max_RTT, 
                                        Data#car_state{ state = stop }}).


compute_position(Data) ->
        % compute car position
        TravelTime = (utils:get_timestamp() - Data#car_state.current_time) / 1000,
        Position = round1f(Data#car_state.position + (Data#car_state.speed * TravelTime * (-1 * Data#car_state.side))),
        utils:log("Compute position: Travel Time (s): ~p, Speed: ~p, OldPosition: ~p, CurrentPosition: ~p, Crossing ~p", [TravelTime, Data#car_state.speed, Data#car_state.position, Position, Data#car_state.crossing]),
        Position.


round1f(Float) ->
    round(Float * 10)/10. 


timeout(State, Target, Data, From) ->
    utils:log("EVENT timeout"), 
    car_call_supervisor_api:car_call({  call_tow_truck, 
                                        Data#car_state.name, 
                                        Target, 
                                        Data#car_state.max_RTT, 
                                        Data#car_state.tow_truck_time}),
    flow:keep(Data, From, {list_to_atom(string:concat(atom_to_list(State),"_timeout")), Data}).


update_front(State, Replacement, Data, From) ->
    utils:log("EVENT update_front"), 
    NewData = Data#car_state{ adj = Data#car_state.adj#adj{front_cars = Replacement} },
    car_call_supervisor_api:car_call({  default_behaviour, 
                                        Data#car_state.name,
                                        Data#car_state.name,
                                        Data#car_state.max_RTT, 
                                        {}}),
    flow:keep(NewData, From, {list_to_atom(string:concat(atom_to_list(State),"_update_front")), NewData}).


update_rear(State, Replacement, Data, From) ->
    utils:log("EVENT update_rear"), 
    NewData = Data#car_state{ adj = Data#car_state.adj#adj{rear_cars = Replacement} },
    car_call_supervisor_api:car_call({  default_behaviour, 
                                        Data#car_state.name,
                                        Data#car_state.name,
                                        Data#car_state.max_RTT, 
                                        {}}),
    flow:keep(NewData, From, {list_to_atom(string:concat(atom_to_list(State),"_update_rear")), NewData}).


check(State, Sender, Data, From) -> 
    utils:log("EVENT check"), 
    NewData = if length(Data#car_state.adj#adj.rear_cars) > 0 ->
        [Rear | _Rest] = Data#car_state.adj#adj.rear_cars,
        if Rear#car_state.name == Sender#car_state.name ->
            utils:log("Car: Rear adj unchanged"),
            Data#car_state{current_time = utils:get_timestamp()};
        true ->
            if Sender#car_state.state == sync ->
                utils:log("Car: detected conflict during sync"),
                Data#car_state{current_time = utils:get_timestamp()};
            true ->
                utils:log("Car: Rear adj update from ~p to ~p", [Data#car_state.adj#adj.rear_cars, Sender]),
                Data#car_state{adj = Data#car_state.adj#adj{ rear_cars = [Sender] }, current_time = utils:get_timestamp()}
            end
        end;
    true ->
        utils:log("Car: Rear adj update from [] to ~p", [Sender]),
        Data#car_state{adj = Data#car_state.adj#adj{rear_cars = [Sender]}, current_time = utils:get_timestamp()}
    end,
    flow:keep(NewData, From, {list_to_atom(string:concat(atom_to_list(State),"_check")), NewData}).


crossing(State, Body, Data, From) ->
    utils:log("EVENT crossing"), 

    if Body#car_state.arrival_time >= State#car_state.arrival_time; Body#car_state.arrival_time < 0 ->
        utils:log("Car: can cross the bridge, propagate"),
        propagate_crossing(Data, Body),
    
        if Data#car_state.crossing ->
            flow:keep(Data, From, {list_to_atom(string:concat(atom_to_list(State),"_crossing")), Data});
        true ->
            NewData = Data#car_state{
                                        crossing = true, 
                                        position = Data#car_state.position + (Data#car_state.bridge_length * Data#car_state.side)
                                    },
            flow:keep(NewData, From, {list_to_atom(string:concat(atom_to_list(State),"_crossing")), NewData})
        end
    end.


propagate_crossing(Data, Body) -> 
    utils:log("TODO 3 ~p ~p", [Data#car_state.adj#adj.rear_cars, Data#car_state.bridge_length]),
    RearCars = utils:first_elements(Data#car_state.adj#adj.rear_cars, Data#car_state.bridge_length),
    utils:log("TODO 4"),
    propagate_crossing_wrapper(Data, RearCars, Body).

propagate_crossing_wrapper(Data, RearCars, Body) -> 
    Sender = Data#car_state.name,
    RTT = Data#car_state.max_RTT,
    utils:log("TODO 5"),
    if length(RearCars) > 0, Body#car_state.bridge_capacity > 0 ->
        utils:log("TODO 6"),
        [Pivot | Rest] = RearCars,
        utils:log("TODO 65"),
        NewBody = Body#car_state{bridge_capacity = Body#car_state.bridge_capacity - 1},
        utils:log("TODO 7"),
        car_call_supervisor_api:car_call({crossing, Sender, Pivot#car_state.name, RTT, NewBody}),
        utils:log("TODO 8"),
        propagate_crossing_wrapper(Data, Rest, NewBody);
    true ->
        utils:log("Propagation ends")
    end.


%crash(_CrashType, Data, From) -> 
%    utils:log("EVENT crash"),
%    flow:next(dead, Data, From, {dead, Data}).


