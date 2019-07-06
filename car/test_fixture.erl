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
        speed = 0,
        crossing = false,
        adj = #adj{front_cars = [], rear_cars = []}, 
        arrival_time = utils:get_timestamp(), 
        state = init,
        bridge_capacity = 5, 
        bridge_length = 3,
        max_speed = Env#env.max_speed,
        tow_truck_time = Env#env.tow_truck_time,
        max_RTT = Env#env.max_RTT
    }.


% car with car2 on the same side in position - 1
default_state2() ->
    (default_state())#car_state{adj = #adj{ front_cars = [ #car_state{ name = car2, side = -1, position = -1 } ]}}.


% car with car2 on the opposite side
default_state3() ->
    (default_state())#car_state{adj = #adj{ front_cars = [ #car_state{ name = car2, side = 1, position = 1 } ]}}.


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


%%%===================================================================
%%% Skip sync state
%%%===================================================================

% car alone
skip_sync(State) ->
    car:default_behaviour(State#car_state.name),

    receive
        {car_call, Req} ->
            {Label, Sender, Target, _Body} = Req,
            case Label of 
                adj ->
                    Adj = #adj{front_cars = [], rear_cars = []},
                    car:adj_response({Label, Sender, Target, utils:get_timestamp(), 0, Adj})
            end
    end.


% car with car2 on the same side in position - 1
skip_sync2(State) ->
    car:default_behaviour(State#car_state.name),
    receive
        {car_call, _Request2} ->
            car:check_response({check, car2, car1, utils:get_timestamp(), 0, State#car_state{current_time = utils:get_timestamp()}})
    end,
    receive
        {car_call, Req} ->
            {Label, Sender, Target, _Body} = Req,
            case Label of 
                adj ->
                    Adj = #adj{front_cars = [#car_state{ name = car2, side = -1, position = 0 }], rear_cars = []},
                    car:adj_response({Label, Sender, Target, utils:get_timestamp(), 0, Adj})
            end
    end.


% car with car2 on the opposite side
skip_sync3(State) ->
    skip_sync2(State).


%%%===================================================================
%%% Skip normal state
%%%===================================================================

skip_normal(_State) ->    
    receive
        {car_call, Req1} ->
            {Label1, Sender1, _Target1, _Body1} = Req1,
            case Label1 of 
                next ->
                    car:default_behaviour(Sender1)
            end
    end,
    receive
        {car_call, Req2} ->
            {Label2, _Sender2, _Target2, _Body2} = Req2,
            case Label2 of 
                wait ->
                    flow:launch_event(timer, [Req2])
            end
    end,
    receive
        {car_call, Req3} ->
            {Label3, Sender3, _Target3, _Body3} = Req3,
            case Label3 of 
                wait_response ->
                    car:default_behaviour(Sender3)
            end
    end.


skip_normal2(_State) ->
    todo.     


skip_normal3(_State) ->
    todo. 


%%%===================================================================
%%% Skip leader state
%%%===================================================================

skip_leader(_State) ->   
    todo.


skip_leader2(_State) ->   
    todo.


skip_leader3(_State) ->   
    todo.