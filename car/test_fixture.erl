%%%===================================================================
%%% Fixture for test initialization
%%%===================================================================


-module(test_fixture).
-compile(export_all).
-include("car.hrl").


% car alone
default_state() ->
    Env = utils:load_environment(),
    #car_state{
        name = car1, 
        side = -1, 
        power = 2, 
        size = 1,
        speed = 0,
        % position undefined
        crossing = false,
        synchronized = false,
        crash_type = 0,
        delta = 0,
        arrival_time = utils:get_timestamp(),
        current_time = utils:get_timestamp(), 
        adj = #adj{front_cars = [], rear_cars = []}, 
        state = sync,
        % settings
        host = Env#env.host,
        bridge_capacity = Env#env.bridge_capacity, 
        bridge_length = Env#env.bridge_length,
        max_speed = Env#env.max_speed,
        tow_truck_time = Env#env.tow_truck_time,
        max_RTT = Env#env.max_RTT
    }.


% car with car2 on the same side in position - 1
default_state2() ->
    (default_state())#car_state{adj = #adj{ front_cars = [ #car_state{ name = car1, side = -1, position = -1, size = 1 } ]}}.


% car with car2 on the opposite side
default_state3() ->
    (default_state())#car_state{adj = #adj{ front_cars = [ #car_state{ name = car1, side = 1, position = 1, size = 1 } ]}}.


register() ->
    case whereis(supervisor) of
        undefined ->
            register(supervisor, self());
        _ ->
            ok
    end.


assert(CurrentResult, ExpectedResult) ->
    if CurrentResult == ExpectedResult ->
        ok;
    true -> 
        throw("test fail")
    end.


listen(Label, Handler) ->
    receive
        {car_call, Req} ->
            utils:log("Test: receive car_call ~p ", [Req]),
            {ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody} = Req,
            case ReqLabel of 
                Label ->
                    utils:log("Test: result ~p ", [Handler(ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody)])
            end;
        {car_reply, Reply} ->
            utils:log("Test: receive car_reply ~p ", [Reply]),
            {ReplyLabel, ReplySender, ReplyTarget, ReplyNickname, ReplyRTT, ReplyBody} = Reply,
            case ReplyLabel of 
                Label ->
                    utils:log("Test: result ~p ", [Handler(ReplyLabel, ReplySender, ReplyTarget, ReplyNickname, ReplyRTT, ReplyBody)])
            end;
        {timer_call, Req} ->
            utils:log("Test: receive timer_call ~p ", [Req]),
            {ReqLabel, ReqSender, ReqTarget, ReqNickname, ReqSendingTime, ReqBody} = Req,
            case ReqLabel of 
                Label ->
                    utils:log("Test: result ~p ", [Handler(ReqLabel, ReqSender, ReqTarget, ReqNickname, ReqSendingTime, ReqBody)])
            end;
        {timer_reply, Reply} ->
            utils:log("Test: receive timer_reply ~p ", [Reply]),
            {ReplyLabel, ReplySender, ReplyTarget, ReplySendingTime, ReplyBody} = Reply,
            case ReplyLabel of 
                Label ->
                    utils:log("Test: result ~p ", [Handler(ReplyLabel, ReplySender, ReplyTarget, ReplySendingTime, ReplyBody)])
            end;   
        Any ->
            utils:log("Test: receive unhandled call ~p ", [Any])
    end.

%%%===================================================================
%%% Skip sync state
%%%===================================================================

% car alone
skip_sync(State) ->
    car:default_behaviour(State#car_state.name),

    receive
        {car_call, Req} ->
            {Label, Sender, Target, _RTT, _Body} = Req,
            case Label of 
                adj ->
                    utils:log("Fixture: return adj"),
                    Adj = #adj{front_cars = [], rear_cars = []},
                    car:adj_reply(Sender, Adj)
            end
    end.


% car with car2 on the same side in position - 1
skip_sync2(State) ->
    car:default_behaviour(State#car_state.name),
    receive
        {car_call, _Request2} ->
            car:check_reply({car2, car1, utils:get_timestamp(), 0, State#car_state{current_time = utils:get_timestamp()}})
    end,
    receive
        {car_call, Req} ->
            {Label, Sender, Target, _RTT, _Body} = Req,
            case Label of 
                adj ->
                    Adj = #adj{front_cars = [#car_state{ name = car1, side = -1, position = 0, size = 1 }], rear_cars = []},
                    car:adj_reply(Sender, Adj)
            end
    end.


% car with car2 on the opposite side
skip_sync3(State) ->
    car:default_behaviour(State#car_state.name),
    receive
        {car_call, _Request2} ->
            car:check_reply({car2, car1, utils:get_timestamp(), 0, State#car_state{current_time = utils:get_timestamp()}})
    end,
    receive
        {car_call, Req} ->
            {Label, Sender, Target, _RTT, _Body} = Req,
            case Label of 
                adj ->
                    Adj = #adj{front_cars = [#car_state{ name = car1, side = 1, position = 0, size = 1 }], rear_cars = []},
                    car:adj_reply(Sender, Adj)
            end
    end.


% car with car2 on the opposite side
skip_to_dead(State, CrashType) ->
    car:default_behaviour(State#car_state.name),
    receive
        {car_call, _Req1} ->
            car:check_reply({car2, car1, utils:get_timestamp(), 0, State#car_state{current_time = utils:get_timestamp()}})
    end,
    receive
        {car_call, Req2} ->
            {Label2, Sender2, Target2, _RTT2, _Body2} = Req2,
            case Label2 of 
                adj ->
                    Adj = #adj{front_cars = [#car_state{ name = car1, side = 1, position = 0, size = 1 }], rear_cars = []},
                    car:adj_reply(Sender2, Adj)
            end
    end,
    receive
        {car_call, Req3} ->
            {Label3, Sender3, _Target3, _RTT3, _Body3} = Req3,
            case Label3 of 
                next ->
                    car:crash(State#car_state.name, CrashType)
            end
    end.


%%%===================================================================
%%% Skip normal state
%%%===================================================================

skip_normal(_State) ->    
    receive
        {car_call, Req1} ->
            {Label1, Sender1, _Target1, _RTT1, _Body1} = Req1,
            case Label1 of 
                next ->
                    car:default_behaviour(Sender1)
            end
    end.


skip_normal2(_State) ->
    todo.     


skip_normal3(_State) ->
    receive
        {car_call, Req1} ->
            {Label1, Sender1, _Target1, _RTT1, _Body1} = Req1,
            case Label1 of 
                next ->
                    car:default_behaviour(Sender1)
            end
    end,
    receive
        {car_call, Req2} ->
            {Label2, _Sender2, _Target2, _RTT2, _Body2} = Req2,
            case Label2 of 
                wait ->
                    flow:launch_event(timer, [Req2])
            end
    end,
    receive
        {car_call, Req3} ->
            {Label3, Sender3, _Target3, _RTT3, _Body3} = Req3,
            case Label3 of 
                wait_reply ->
                    car:default_behaviour(Sender3)
            end
    end.

%%%===================================================================
%%% Skip leader state
%%%===================================================================

skip_leader(_State) ->   
    todo.


skip_leader2(_State) ->   
    todo.


skip_leader3(_State) ->   
    todo.