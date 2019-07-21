-module(simulation_service).
-compile(export_all).
-include("entity.hrl").


%%%===================================================================
%%% public functions
%%%===================================================================

init(Entity) ->
    settings_repository:add(Entity).


state() ->
    adj_marshalling(adj_repository:get_all()).


new(Entity) ->
    [Settings] = settings_repository:get_all(),
    utils:log("Result ~p", [Settings]),
    
    utils:log("Visibility ~p", [Settings#settingsEntity.process_visibility]),
    case Settings#settingsEntity.process_visibility of 
        visible ->
            os:cmd(utils:concat([   "cd ../../../../../car;gnome-terminal -e 'erl -sname ", 
                                    Entity#newCarEntity.name, 
                                    " -setcookie distributed-system-project"
                                    " -run car_supervisor start ",
                                    Entity#newCarEntity.host, " ",
                                    lists:flatten(io_lib:format("~p", [Entity#newCarEntity.port])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#newCarEntity.side + 1])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#newCarEntity.power])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#newCarEntity.size])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settingsEntity.bridge_capacity])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settingsEntity.bridge_length])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settingsEntity.max_speed])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settingsEntity.tow_truck_time])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settingsEntity.max_RTT])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#newCarEntity.crash_type])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#newCarEntity.timeout])),
                                    "' || true"]));
        detached -> 
            os:cmd(utils:concat([   "cd ../../../../../car;erl -detached -sname ", 
                                    Entity#newCarEntity.name, 
                                    " -setcookie distributed-system-project"
                                    " -run car_supervisor start ",
                                    Entity#newCarEntity.host, " ",
                                    Entity#newCarEntity.port, " ",
                                    lists:flatten(io_lib:format("~p", [Entity#newCarEntity.side + 1])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#newCarEntity.power])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#newCarEntity.size])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settingsEntity.bridge_capacity])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settingsEntity.bridge_length])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settingsEntity.max_speed])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settingsEntity.tow_truck_time])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settingsEntity.max_RTT])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#newCarEntity.crash_type])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#newCarEntity.timeout]))]));
        docker -> 
            utils:log(utils:concat([   "sudo docker run --net ds_network -e host='", 
                                        Entity#newCarEntity.host, "' -e port='", 
                                        Entity#newCarEntity.port, "' -e name='",
                                        Entity#newCarEntity.name, "' -e side=",
                                        lists:flatten(io_lib:format("~p", [Entity#newCarEntity.side + 1])), " -e power=",
                                        lists:flatten(io_lib:format("~p", [Entity#newCarEntity.power])), " -e size=",
                                        lists:flatten(io_lib:format("~p", [Entity#newCarEntity.size])), " -e bridge_capacity=",
                                        lists:flatten(io_lib:format("~p", [Settings#settingsEntity.bridge_capacity])), " -e bridge_length=",
                                        lists:flatten(io_lib:format("~p", [Settings#settingsEntity.bridge_length])), " -e max_speed=",
                                        lists:flatten(io_lib:format("~p", [Settings#settingsEntity.max_speed])), " -e tow_truck_time=",
                                        lists:flatten(io_lib:format("~p", [Settings#settingsEntity.tow_truck_time])), " -e max_RTT=",
                                        lists:flatten(io_lib:format("~p", [Settings#settingsEntity.max_RTT])), " -e crash_type=",
                                        lists:flatten(io_lib:format("~p", [Entity#newCarEntity.crash_type])), " -e timeout=",
                                        lists:flatten(io_lib:format("~p", [Entity#newCarEntity.timeout])),
                                        " -dt car:v1"]))
    end,
    car_marshalling(#carEntity{ name = list_to_atom(Entity#newCarEntity.name), 
                                side = Entity#newCarEntity.side, 
                                power = Entity#newCarEntity.power, 
                                size = Entity#newCarEntity.size, 
                                timeout = Entity#newCarEntity.timeout,
                                max_speed = Settings#settingsEntity.max_speed,
                                max_RTT = Settings#settingsEntity.max_RTT,
                                tow_truck_time = Settings#settingsEntity.tow_truck_time,
                                bridge_capacity = Settings#settingsEntity.bridge_capacity,
                                bridge_length = Settings#settingsEntity.bridge_length }).

reset() ->
    % TODO kill docker 
    os:cmd("for i in `ps -ef | grep car | awk '{print $2}'`; do echo $i; kill -9 $i; done"),
    os:cmd("for i in `sudo docker ps | grep car | awk '{print $1}'`; do echo $i; sudo docker stop $i; done"),
    settings_repository:reset().
                       

%%%===================================================================
%%% private functions
%%%===================================================================