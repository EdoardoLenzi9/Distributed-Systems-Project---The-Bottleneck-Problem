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
    {ok, sync, State}.
        

sync({call, From}, Event, Data) ->
    utils:log("STATE Sync"),
    case Event of  
    {timeout, Target} ->  
        timeout(sync, Target, Data, From);
    {update_front, Replacement} ->  
        update_front(sync, Replacement, Data, From);
    {update_rear, Replacement} ->  
        update_rear(sync, Replacement, Data, From);
    check ->  
        check(sync, Data, From);
    {crossing, Req} ->   
        crossing(sync, Req, Data, From);
    {crash, CrashType} ->   
        utils:log("EVENT crash (postpone)"), 
        NewData = Data#car_state{ crash_type = CrashType },
        flow:keep(NewData, From, {sync_crash, NewData});
    {check_reply, Reply} ->
        utils:log("EVENT check_reply"),
        % berkeley
        {_Sender, _Target, SendingTime, RTT, Body} = Reply,
        CurrentTime = SendingTime, 
        PivotTime = Body#car_state.current_time,
        Delta = CurrentTime - (PivotTime + RTT / 2),
        NewData = Data#car_state{delta = Delta, arrival_time = Data#car_state.arrival_time + Delta},
        car_call_supervisor_api:car_call({adj, NewData#car_state.name, none, NewData#car_state.max_RTT, NewData}),
        flow:keep(NewData, From, {sync_check_reply, NewData});
    {adj_reply, Adj} ->
        utils:log("EVENT adj_reply"),
        NewData = Data#car_state{adj = Adj},
        utils:log("~p", [NewData]),
        Position = if length(NewData#car_state.adj#adj.front_cars) > 0 ->
            [First | _Rest] = NewData#car_state.adj#adj.front_cars,
            % if there is any car in the front queue and on the same side
            if First#car_state.side == NewData#car_state.side ->
                First#car_state.position + (((Data#car_state.size / 2) + (First#car_state.size / 2)) * Data#car_state.side); 
            % if there are only cars on the opposite side
            true -> 
                Data#car_state.side * Data#car_state.size / 2 
            end;
            % if there isn't any other car in the front queue
        true ->
            Data#car_state.side * Data#car_state.size / 2 
        end,
        utils:log("initial position: ~p", [Position]),
        NewData2 = NewData#car_state{speed = 0, position = Position, current_time = utils:get_timestamp(), synchronized = true},
        flow:next(normal, NewData2, From, {normal, NewData2});
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
                                                {}
                                            }),
            flow:keep(Data, From, {sync_default_behaviour, Data});
        true ->
            car_call_supervisor_api:car_call({adj, Data#car_state.name, none, Data#car_state.max_RTT, Data})
        end,
        flow:keep(Data, From, {sync_default_behaviour, Data});
    Event ->
        ignore(sync, Event, Data, From)
    end.


normal({call, From}, Event, Data) ->
    utils:log("STATE Normal"),
    case Event of   
        {timeout, Target} ->  
            timeout(normal, Target, Data, From);
        {crash, CrashType} ->
            crash(CrashType, Data, From);
        {update_front, Replacement} ->  
            update_front(sync, Replacement, Data, From);
        {update_rear, Replacement} ->  
            update_rear(sync, Replacement, Data, From);
        {crossing, Req} ->   
            crossing(normal, Req, Data, From);    
        check ->  
            check(normal,Data, From);
        {check_reply, Reply} ->
            utils:log("EVENT check_reply"),
            {_Sender, _Target, _SendingTime, _RTT, Body} = Reply,

            Position = compute_position(Data),

            if (Position * Data#car_state.side) =< (Data#car_state.size / 2) ->
                NewData = Data#car_state{position = (Data#car_state.size * Data#car_state.side / 2), speed = 0},
                if Data#car_state.crossing ->
                    utils:log("Car: reach the end of the bridge"),
                    flow:next(dead, NewData, From, {dead, NewData});
                true ->
                    % if car reaches the bridge go to leader state
                    utils:log("Car: reach the bridge"),
                    NewData = Data#car_state{position = (Data#car_state.size * Data#car_state.side / 2), crossing = true, speed = 0},
                    flow:next(leader, NewData, From, {leader, NewData})
                end;
            % otherwise continue polling
            true ->
                utils:log("Car: away from the bridge"),
                Offset = ((Data#car_state.size / 2) + (Body#car_state.size / 2)) * Data#car_state.side,
                Distance = if Body#car_state.crossing, Data#car_state.crossing ->
                    (Data#car_state.position - (Body#car_state.position - (Data#car_state.bridge_length * Body#car_state.side)) - Offset) * Data#car_state.side;
                Body#car_state.crossing ->
                    erlang:min((Data#car_state.size / 2), 
                               (Data#car_state.position - (Body#car_state.position - (Data#car_state.bridge_length * Body#car_state.side)) - Offset) * Data#car_state.side);
                true ->
                    (Data#car_state.position - Body#car_state.position - Offset) * Data#car_state.side
                end,
                Speed = Distance  / (Data#car_state.max_RTT / 1000),
                
                utils:log("Car: travel with position ~p, speed ~p, distance ~p, offset ~p", [Position, Speed, Distance, Offset]),
                
                % continue polling
                car_call_supervisor_api:car_call({wait, Data#car_state.name, none, Data#car_state.max_RTT, Data#car_state.max_RTT}),

                NewData = Data#car_state{position = Position, speed = Speed, current_time = utils:get_timestamp() },
                car_call_supervisor_api:car_call({log_state, NewData#car_state.name, none, NewData#car_state.max_RTT, NewData}),
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
                NewData = Data#car_state{position = (Data#car_state.size * Data#car_state.side / 2), speed = 0},
                if Data#car_state.crossing ->
                    utils:log("Car: reach the end of the bridge"),
                    flow:next(dead, NewData, From, {dead, NewData});
                true ->
                    % if car reaches the bridge go to leader state
                    utils:log("Car: reach the bridge"),
                    NewData2 = Data#car_state{position = (Data#car_state.size * Data#car_state.side / 2), crossing = true, speed = 0},
                    flow:next(leader, NewData2, From, {leader, NewData2})
                end;
            true ->
                if Data#car_state.crossing ->
                    utils:log("Car: on the bridge"),
                    NewData = Data#car_state{position = Position},    
                    car_call_supervisor_api:car_call({log_state, NewData#car_state.name, none, NewData#car_state.max_RTT, NewData}),
                    car_call_supervisor_api:car_call({wait, NewData#car_state.name, none, NewData#car_state.max_RTT, NewData#car_state.max_RTT}),
                    flow:keep(NewData, From, {normal_check_reply, NewData});
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
                            Speed = erlang:min((((Data#car_state.position * Data#car_state.side) - (Data#car_state.size / 2)) / (Data#car_state.max_RTT / 1000)), Data#car_state.max_speed),
                            utils:log("New speed ~p", [Speed]),
                            car_call_supervisor_api:car_call({wait, Data#car_state.name, none, Data#car_state.max_RTT, Data#car_state.max_RTT}),
                            Data#car_state{speed = Speed}
                        end;
                    % if there isn't any other car 
                    true ->
                        utils:log("Car: there isn't any other car in the front queue"),
                        Speed = erlang:min((((Data#car_state.position * Data#car_state.side) - (Data#car_state.size / 2)) / (Data#car_state.max_RTT / 1000)), Data#car_state.max_speed),
                        utils:log("New speed2 ~p", [Speed]),
                        car_call_supervisor_api:car_call({wait, Data#car_state.name, none, Data#car_state.max_RTT, Data#car_state.max_RTT}),
                        Data#car_state{speed = Speed}
                    end,
                    car_call_supervisor_api:car_call({log_state, NewData#car_state.name, none, NewData#car_state.max_RTT, NewData}),
                    flow:keep(NewData, From, {normal_default_behaviour, Data})
                end
            end;
        Event ->
            ignore(normal, Event, Data, From)
    end.


leader({call, From}, Event, Data) ->
    utils:log("STATE Leader"),
    case Event of
        {timeout, Target} ->  
            timeout(leader, Target, Data, From);
        {crash, CrashType} ->
            crash(CrashType, Data, From);
        {update_front, Replacement} ->  
            update_front(sync, Replacement, Data, From);
        {update_rear, Replacement} ->  
            update_rear(sync, Replacement, Data, From);
        check ->  
            check(leader, Data, From);
        {check_reply, Reply} ->
            utils:log("EVENT check_reply"),
            {_Sender, _Target, _SendingTime, _RTT, Body} = Reply, 

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
        default_behaviour ->
            FrontCars = Data#car_state.adj#adj.front_cars,
            if length(FrontCars) > 0 ->
                utils:log("There is a car on the opposite side of the bridge"),
                [Pivot|_Rest] = FrontCars,
                car_call_supervisor_api:car_call({check, Data#car_state.name, Pivot#car_state.name, Data#car_state.max_RTT, {}}),
                flow:keep(Data, From, {leader_default_behaviour, Data});
            true ->
                utils:log("the car can start crossing the bridge"),
                NewData = Data#car_state{   
                                            position = Data#car_state.bridge_length * Data#car_state.side, 
                                            crossing = true, 
                                            speed = Data#car_state.max_speed
                                        },
                propagate_crossing(Data, #car_state{arrival_time = -1, bridge_capacity = Data#car_state.bridge_capacity}),
                flow:next(normal, NewData, From, {normal, NewData})
            end;
        Event ->
            ignore(leader, Event, Data, From)
    end.


dead({call, From}, Event, Data) -> 
    utils:log("STATE Dead"),
    case Event of    
        tow_truck ->
            utils:log("EVENT tow_truck"),
            notify_dead_and_stop(Data),
            flow:keep(Data, From, {dead_tow_truck, Data});
        default_behaviour ->
            utils:log("EVENT default_behaviour"),
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
            ignore(dead, Event, Data, From)
    end.


%%%===================================================================
%%% Utility functions
%%%===================================================================


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
                                            RearCar});
    true ->
        ok
    end,
    if RearCar =/= [] ->
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
        utils:log("Compute position: Travel Time (s): ~p, Speed: ~p, OldPosition: ~p, CurrentPosition: ~p", [TravelTime, Data#car_state.speed, Data#car_state.position, Position]),
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


check(State, Data, From) -> 
    utils:log("EVENT check"), 
    NewData = Data#car_state{current_time = utils:get_timestamp()},
    flow:keep(NewData, From, {list_to_atom(string:concat(atom_to_list(State),"_check")), NewData}).


crossing(State, Req, Data, From) ->
    utils:log("EVENT crossing"), 
    {_Label, _Sender, _Target, _Nickname, _SendingTime, Body} = Req,

    if Body#car_state.arrival_time >= State#car_state.arrival_time; Body#car_state.arrival_time < 0 ->
        utils:log("Car: can cross the bridge, propagate"),
        propagate_crossing(Data, Req),
    
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
    RearCars = utils:first_elements(Data#car_state.adj#adj.rear_cars, Data#car_state.bridge_length),
    propagate_crossing_wrapper(Data#car_state.name, RearCars, Body).

propagate_crossing_wrapper(Sender, RearCars, Body) -> 
    if length(RearCars) > 0, Body#car_state.bridge_capacity > 0 ->
        [Pivot | Rest] = RearCars,
        NewBody = Body#car_state{bridge_capacity = Body#car_state.bridge_capacity - 1},
        car_call_supervisor_api:car_call({crossing, Sender, Pivot#car_state.name, NewBody}),
        propagate_crossing_wrapper(Sender, Rest, NewBody);
    true ->
        utils:log("Propagation ends")
    end.


crash(CrashType, Data, From) -> 
    utils:log("EVENT crash"),
    NewData = Data#car_state{ crash_type = CrashType },
    flow:next(dead, NewData, From, {dead, NewData}).


ignore(State, Event, Data, From) ->
    utils:log("Ignore unhandled events", [Event]),
    flow:keep(Data, From, {list_to_atom(string:concat(atom_to_list(State),"_ignore")), Data}).