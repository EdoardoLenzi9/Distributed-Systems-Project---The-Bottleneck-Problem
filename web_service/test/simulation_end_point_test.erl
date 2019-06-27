-module(simulation_end_point_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


init_test_() ->
    db_manager:start(),
    Params = "{\"turn\":1000,\"bridgeCapacity\":2,\"bridgeCrossingTime\":2500}",
    Res = <<"{\"result\":\"success\"}">>,
    [   ?_assert(simulation_controller:init_handler(Params) =:= Res) ].
