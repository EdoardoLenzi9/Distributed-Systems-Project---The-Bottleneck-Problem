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
        turn = 1000,
        bridge_capacity = 5, 
        bridge_length = 3,
        max_speed = Env#env.max_speed,
        tow_truck_time = Env#env.tow_truck_time,
        max_RTT = Env#env.max_RTT
    }.


% car with car2 on the same side
default_state2() ->
    (default_state())#car_state{adj = #adj{ front_cars = [ #car_state{ name = car2, side = -1 } ]}}.


% car with car2 on the opposite side
default_state3() ->
    (default_state())#car_state{adj = #adj{ front_cars = [ #car_state{ name = car2, side = 1 } ]}}.


register() ->
    case whereis(supervisor) of
        undefined ->
            register(supervisor, self());
        _ ->
            ok
    end.


skip_sync(State) ->
    car:default_behaviour(State#car_state.name),

    receive
        {car_call, Req} ->
            {Label, Sender, Target, Body} = Req,
            case Label of 
                adj ->
                    Adj = #adj{front_cars = [], rear_cars = []},
                    car:adj_response({Label, Sender, Target, utils:get_timestamp(), 0, Adj})
            end
    end.


skip_sync2(State) ->
    car:default_behaviour(State#car_state.name),
    receive
        {car_call, Request2} ->
            car:check_response({check, car2, car1, utils:get_timestamp(), 0, State#car_state{current_time = utils:get_timestamp()}})
    end,
    receive
        {car_call, Req} ->
            {Label, Sender, Target, Body} = Req,
            case Label of 
                adj ->
                    Adj = #adj{front_cars = [#car_state{ name = car2, side = -1, position = 0 }], rear_cars = []},
                    car:adj_response({Label, Sender, Target, utils:get_timestamp(), 0, Adj})
            end
    end.