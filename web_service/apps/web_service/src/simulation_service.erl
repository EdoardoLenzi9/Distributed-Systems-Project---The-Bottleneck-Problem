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
    utils:log("~p", [utils:concat([   "cd ../../../../../car;gnome-terminal -e 'erl -name ", 
        Entity#newCarEntity.name, 
        "@", 
        Entity#newCarEntity.name, 
        " -run car_supervisor start ",
        Entity#newCarEntity.name, " ",
        lists:flatten(io_lib:format("~p", [Entity#newCarEntity.side])), " ",
        lists:flatten(io_lib:format("~p", [Entity#newCarEntity.power])), " ",
        lists:flatten(io_lib:format("~p", [Entity#newCarEntity.size])), " ",
        lists:flatten(io_lib:format("~p", [Settings#settingsEntity.max_speed])), " ",
        lists:flatten(io_lib:format("~p", [Settings#settingsEntity.max_RTT])), " ",
        lists:flatten(io_lib:format("~p", [Settings#settingsEntity.tow_truck_time])), " ",
        lists:flatten(io_lib:format("~p", [Settings#settingsEntity.bridge_capacity])), " ",
        lists:flatten(io_lib:format("~p", [Settings#settingsEntity.bridge_length])), " ",
        lists:flatten(io_lib:format("~p", [Entity#newCarEntity.timeout])),
        "'"])]),
    os:cmd(utils:concat([   "cd ../../../../../car;gnome-terminal -e 'erl -name ", 
                            Entity#newCarEntity.name, 
                            "@", 
                            Entity#newCarEntity.name, 
                            " -run car_supervisor start ",
                            Entity#newCarEntity.name, " ",
                            lists:flatten(io_lib:format("~p", [Entity#newCarEntity.side])), " ",
                            lists:flatten(io_lib:format("~p", [Entity#newCarEntity.power])), " ",
                            lists:flatten(io_lib:format("~p", [Entity#newCarEntity.size])), " ",
                            lists:flatten(io_lib:format("~p", [Settings#settingsEntity.max_speed])), " ",
                            lists:flatten(io_lib:format("~p", [Settings#settingsEntity.max_RTT])), " ",
                            lists:flatten(io_lib:format("~p", [Settings#settingsEntity.tow_truck_time])), " ",
                            lists:flatten(io_lib:format("~p", [Settings#settingsEntity.bridge_capacity])), " ",
                            lists:flatten(io_lib:format("~p", [Settings#settingsEntity.bridge_length])), " ",
                            lists:flatten(io_lib:format("~p", [Entity#newCarEntity.timeout])),
                            "'"])),
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
    settings_repository:reset().
                        

%%%===================================================================
%%% private functions
%%%===================================================================