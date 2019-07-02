-module(test_fixture).
-compile(export_all).
-include("car.hrl").


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


register() ->
    case whereis(supervisor) of
        undefined ->
            register(supervisor, self());
        _ ->
            ok
    end.