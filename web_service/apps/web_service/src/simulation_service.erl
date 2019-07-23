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
    Hosts = host_repository:get_all(),

    utils:log("Settings ~p", [Settings]),
    utils:log("Hosts ~p", [Hosts]),
    
    [SelectedHost | _Rest] = Hosts,
    host_repository:add(SelectedHost#host_entity{ number_of_cars = SelectedHost#host_entity.number_of_cars + 1}),

    utils:log("Visibility ~p", [Settings#settings_entity.process_visibility]),
    case Settings#settings_entity.process_visibility of 
        visible ->
            Command = utils:concat([   "cd ../../../../../car;gnome-terminal -e 'erl -sname ", 
                                    Entity#new_car_entity.name, 
                                    " -setcookie distributed-system-project"
                                    " -run car_supervisor start ",
                                    Entity#new_car_entity.host, " ",
                                    lists:flatten(io_lib:format("~p", [Entity#new_car_entity.port])), " ",
                                    SelectedHost#host_entity.host, " ",
                                    SelectedHost#host_entity.ip, " ",
                                    lists:flatten(io_lib:format("~p", [Entity#new_car_entity.side + 1])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#new_car_entity.power])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#new_car_entity.size])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settings_entity.bridge_capacity])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settings_entity.bridge_length])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settings_entity.max_speed])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settings_entity.tow_truck_time])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settings_entity.max_RTT])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#new_car_entity.crash_type])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#new_car_entity.timeout])),
                                    "' || true"]),
            os:cmd(Command),
            utils:log(Command);
        detached -> 
            os:cmd(utils:concat([   "cd ../../../../../car;erl -detached -sname ", 
                                    Entity#new_car_entity.name, 
                                    " -setcookie distributed-system-project"
                                    " -run car_supervisor start ",
                                    Entity#new_car_entity.host, " ",
                                    Entity#new_car_entity.port, " ",
                                    SelectedHost#host_entity.host, " ",
                                    SelectedHost#host_entity.ip, " ",
                                    lists:flatten(io_lib:format("~p", [Entity#new_car_entity.side + 1])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#new_car_entity.power])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#new_car_entity.size])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settings_entity.bridge_capacity])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settings_entity.bridge_length])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settings_entity.max_speed])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settings_entity.tow_truck_time])), " ",
                                    lists:flatten(io_lib:format("~p", [Settings#settings_entity.max_RTT])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#new_car_entity.crash_type])), " ",
                                    lists:flatten(io_lib:format("~p", [Entity#new_car_entity.timeout]))]));
        docker -> 
            Command = utils:concat([   
                                        "sshpass -p '", 
                                        SelectedHost#host_entity.password, 
                                        "' ssh -o \"StrictHostKeyChecking=no\" ",
                                        SelectedHost#host_entity.host, 
                                        "@",
                                        SelectedHost#host_entity.ip, 
                                        " \"nohup sudo docker run --net ds_network -e ws_host='", 
                                        Entity#new_car_entity.host, "' -e ws_port='", 
                                        Entity#new_car_entity.port, "' -e host='",
                                        SelectedHost#host_entity.host, "' -e ip='",
                                        SelectedHost#host_entity.ip, "' -e name='",
                                        Entity#new_car_entity.name, "' -e side=",
                                        lists:flatten(io_lib:format("~p", [Entity#new_car_entity.side + 1])), " -e power=",
                                        lists:flatten(io_lib:format("~p", [Entity#new_car_entity.power])), " -e size=",
                                        lists:flatten(io_lib:format("~p", [Entity#new_car_entity.size])), " -e bridge_capacity=",
                                        lists:flatten(io_lib:format("~p", [Settings#settings_entity.bridge_capacity])), " -e bridge_length=",
                                        lists:flatten(io_lib:format("~p", [Settings#settings_entity.bridge_length])), " -e max_speed=",
                                        lists:flatten(io_lib:format("~p", [Settings#settings_entity.max_speed])), " -e tow_truck_time=",
                                        lists:flatten(io_lib:format("~p", [Settings#settings_entity.tow_truck_time])), " -e max_RTT=",
                                        lists:flatten(io_lib:format("~p", [Settings#settings_entity.max_RTT])), " -e crash_type=",
                                        lists:flatten(io_lib:format("~p", [Entity#new_car_entity.crash_type])), " -e timeout=",
                                        lists:flatten(io_lib:format("~p", [Entity#new_car_entity.timeout])),
                                        " -dt car:v1 &\""
                                    ]),
            utils:log(Command),
            os:cmd(Command)
    end,
    car_marshalling(#car_entity{ name = list_to_atom(Entity#new_car_entity.name), 
                                side = Entity#new_car_entity.side, 
                                power = Entity#new_car_entity.power, 
                                size = Entity#new_car_entity.size, 
                                timeout = Entity#new_car_entity.timeout,
                                max_speed = Settings#settings_entity.max_speed,
                                max_RTT = Settings#settings_entity.max_RTT,
                                tow_truck_time = Settings#settings_entity.tow_truck_time,
                                bridge_capacity = Settings#settings_entity.bridge_capacity,
                                bridge_length = Settings#settings_entity.bridge_length }).
    

reset() ->
    % TODO kill docker 
    os:cmd("for i in `ps -ef | grep car | awk '{print $2}'`; do echo $i; kill -9 $i; done"),
    os:cmd("for i in `sudo docker ps | grep car | awk '{print $1}'`; do echo $i; sudo docker stop $i; done"),
    settings_repository:reset().
                       

%%%===================================================================
%%% private functions
%%%===================================================================