-module(simulation_end_point_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


init_test_() ->
    db_manager:start(),
    Params = "{\"max_speed\":2,\"max_RTT\":1000,\"tow_truck_time\":1500,\"bridge_capacity\":2,\"bridge_length\":2}",
    Res = <<"{\"result\":\"success\"}">>,
    [   ?_assert(simulation_controller:init_handler(Params) =:= Res) ].


new_test_() ->
    db_manager:start(),
    Params = "{\"name\":\"car1\",\"side\":\"left\",\"power\":2,\"size\":1,\"timeout\":4000}",
    Res = <<"{\"name\":\"car1\",\"side\":\"left\",\"power\":2,\"size\":1,\"max_speed\":2,\"bridge_capacity\":2,\"bridge_length\":2,\"tow_truck_time\":1500,\"timeout\":4000}">>,
    [   ?_assert(simulation_controller:new_node_handler(Params) =:= Res) ].

"host": "http://localhost:8090",
"max_speed": 2,				
"bridge_capacity": 2,				
"bridge_length": 2,
"sampling_frequency": 500,		
"tow_truck_time": 1500,
"max_RTT": 1000	