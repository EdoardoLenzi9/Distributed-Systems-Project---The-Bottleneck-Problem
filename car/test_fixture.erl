%%%===================================================================
%%% Fixture for test initialization
%%%===================================================================


-module(test_fixture).
-compile(export_all).
-include("car.hrl").


% car alone

queue_car(Position, Crossing) ->
    #car_state{  
                name = node(), 
                side = -1,
                speed = 0,
                size = 1,
                crossing = Crossing,
                position = Position,
                current_time = utils:get_timestamp()
            }.

default_state() ->
    Env = utils:load_environment(),
    #car_state{
        name = node(), 
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
    (default_state())#car_state{adj = #adj{ front_cars = [ #car_state{ name = node(), side = -1, position = -1, size = 1 } ]}}.


% car with car2 on the opposite side
default_state3() ->
    (default_state())#car_state{adj = #adj{ front_cars = [ #car_state{ name = node(), side = 1, position = 1, size = 1 } ]}}.


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
    listen(adj, fun(_, Sender, _, _, _) -> 
        Adj = #adj{front_cars = [], rear_cars = []},
        {_Result, _Data} = car:adj_reply(Sender, Adj)
    end).


% car with car2 on the same side in position - 1
skip_sync2(State) ->
    car:default_behaviour(State#car_state.name),
    listen(check, fun(_, _, _, _, _) -> 
        car:check_reply({node(), node(), utils:get_timestamp(), 0, State#car_state{current_time = utils:get_timestamp()}})
    end),
    listen(adj, fun(_, Sender, _, _, _) -> 
        Adj = #adj{front_cars = [#car_state{ name = node(), side = -1, position = -1.5, size = 1 }], rear_cars = []},
        car:adj_reply(Sender, Adj)
    end).


% car with car2 on the opposite side
skip_sync3(State) ->
    car:default_behaviour(State#car_state.name),
    listen(check, fun(_, _, _, _, _) -> 
        car:check_reply({node(), node(), utils:get_timestamp(), 0, State#car_state{current_time = utils:get_timestamp()}})
    end),
    listen(adj, fun(_, Sender, _, _, _) -> 
        Adj = #adj{front_cars = [#car_state{ name = node(), side = 1, position = 0, size = 1 }], rear_cars = []},
        car:adj_reply(Sender, Adj)
    end).


%%%===================================================================
%%% Skip normal state
%%%===================================================================

skip_normal(_State) ->    
    skip_next().


skip_normal2(_State) ->
    skip_next(),
    listen(check, fun(_ReqLabel, _ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:check_reply({node(), node(), utils:get_timestamp(), 0, queue_car(0, true)}),
        car:update_front(_ReqSender, [])
    end),
    skip_log_state(),
    listen(check, fun(_ReqLabel, _ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        utils:log("Test: receive fail check")
    end),
    skip_log_state(),
    listen(default_behaviour, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:default_behaviour(ReqSender)
    end),
    listen(wait, fun(ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody) -> 
        flow:launch_event(timer, [{ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody}])
    end),
    skip_log_state(),
    listen(wait_reply, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:default_behaviour(ReqSender)
    end).


skip_normal3(_State) ->
    skip_next().

%%%===================================================================
%%% Skip leader state
%%%===================================================================

skip_leader(_State) ->   
    skip_next().


skip_leader2(_State) ->   
    skip_next().


skip_leader3(_State) ->   
    listen(next, fun(_ReqLabel, ReqSender, _ReqTarget, _ReqRTT, _ReqBody) -> 
        car:default_behaviour(ReqSender)
    end),
    listen(check, fun(ReqLabel, ReqSender, ReqTarget, ReqRTT, ReqBody) -> 
        flow:launch_event(request_timer, [{ReqLabel, ReqSender, ReqTarget, utils:get_timestamp(), ReqRTT, ReqBody}])   
    end),
    listen(check, fun(_ReqLabel, ReqSender, ReqTarget, ReqNickname, ReqSendingTime, _ReqBody) -> 
        {_Result, Data} = car:check(ReqTarget),
        supervisor_reply_supervisor_api:sup_reply({check_reply, ReqTarget, ReqSender, ReqNickname, ReqSendingTime, Data})
    end),
    listen(check_reply, fun(_ReplyLabel, ReplySender, ReplyTarget, ReplySendingTime, ReplyBody) -> 
        RTT = utils:get_timestamp() - ReplySendingTime,
        {_Result, _Data} = car:check_reply({ReplySender, ReplyTarget, ReplySendingTime, RTT, ReplyBody#car_state{arrival_time = ReplySendingTime}})
    end).


skip_next() ->   
    listen(next, fun(_, Sender, _, _, _) -> 
        car:default_behaviour(Sender)
    end).


skip_log_state() ->  
    listen(log_state, fun(_, _, _, _, Body) -> 
        utils:log("~n current state log ~p ~n", [Body])
    end).