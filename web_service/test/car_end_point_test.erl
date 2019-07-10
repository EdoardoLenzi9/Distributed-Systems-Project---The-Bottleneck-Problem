-module(car_end_point_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


sync_test_() ->
    db_manager:start(),
    % 6 3 2 1 [bridge] 4 5
    Car1 = "{\"name\":\"car1\",\"side\":\"left\",\"power\":2}",
    Res1 = <<"[]">>,
    Car2 = "{\"name\":\"car2\",\"side\":\"left\",\"power\":2}",
    Res2 = list_to_binary(utils:concat(["[", Car1, "]"])),
    Car3 = "{\"name\":\"car3\",\"side\":\"left\",\"power\":2}",
    Res3 = list_to_binary(utils:concat(["[", Car2, ",", Car1, "]"])),
    Car4 = "{\"name\":\"car4\",\"side\":\"right\",\"power\":2}",
    Res4 = list_to_binary(utils:concat(["[", Car1, ",", Car2, "]"])),
    Car5 = "{\"name\":\"car5\",\"side\":\"right\",\"power\":2}",
    Res5 = list_to_binary(utils:concat(["[", Car4, ",", Car1, "]"])),
    Car6 = "{\"name\":\"car5\",\"side\":\"left\",\"power\":3}",
    Res6 = list_to_binary(utils:concat(["[", Car3, ",", Car2, ",", Car1, "]"])),
    [   ?_assert(car_controller:sync_handler(Car1) =:= Res1),
        ?_assert(car_controller:sync_handler(Car2) =:= Res2),
        ?_assert(car_controller:sync_handler(Car3) =:= Res3),
        ?_assert(car_controller:sync_handler(Car4) =:= Res4),
        ?_assert(car_controller:sync_handler(Car5) =:= Res5),
        ?_assert(car_controller:sync_handler(Car6) =:= Res6) ].


adj_test_() ->
        db_manager:start(),
        % 3 2 1 [bridge] 4 5
        Car1 = "{\"name\":\"car1\",\"side\":\"left\",\"power\":2,\"arrival_time\":1000,\"delta\":5,\"state\":\"create\"}",
        Res1 = <<"[[],[]]">>,
        Car2 = "{\"name\":\"car2\",\"side\":\"left\",\"power\":2,\"arrival_time\":2000,\"delta\":5,\"state\":\"create\"}",
        Res2 = list_to_binary(utils:concat(["[[", Car1, "],[]]"])),
        Car3 = "{\"name\":\"car3\",\"side\":\"left\",\"power\":2,\"arrival_time\":3000,\"delta\":5,\"state\":\"create\"}",
        Res3 = list_to_binary(utils:concat(["[[", Car2, ",", Car1, "],[]]"])),
        Car4 = "{\"name\":\"car4\",\"side\":\"right\",\"power\":2,\"arrival_time\":4000,\"delta\":5,\"state\":\"create\"}",
        Res4 = list_to_binary(utils:concat(["[[", Car1, ",", Car2, "],[]]"])),
        Car5 = "{\"name\":\"car5\",\"side\":\"right\",\"power\":2,\"arrival_time\":5000,\"delta\":5,\"state\":\"create\"}",
        Res5 = list_to_binary(utils:concat(["[[", Car4, ",", Car1, "],[]]"])),
        % Update state of car1 to leader
        Car1_leader = "{\"name\":\"car1\",\"side\":\"left\",\"power\":2,\"arrival_time\":1000,\"delta\":5,\"state\":\"leader\"}",
        Res6 = list_to_binary(utils:concat(["[[", Car4, ",", Car5, "],[", Car2, ",", Car3, "]]"])),
        % Check state update
        Res7 = list_to_binary(utils:concat(["[[", Car1_leader, ",", Car4, "],[", Car3, "]]"])),
        [   ?_assert(car_controller:adj_handler(Car1) =:= Res1),
            ?_assert(car_controller:adj_handler(Car2) =:= Res2),
            ?_assert(car_controller:adj_handler(Car3) =:= Res3),
            ?_assert(car_controller:adj_handler(Car4) =:= Res4),
            ?_assert(car_controller:adj_handler(Car5) =:= Res5),
            ?_assert(car_controller:adj_handler(Car1_leader) =:= Res6),
            ?_assert(car_controller:adj_handler(Car2) =:= Res7) ].