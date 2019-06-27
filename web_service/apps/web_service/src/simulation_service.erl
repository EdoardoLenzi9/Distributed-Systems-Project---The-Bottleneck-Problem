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
    %os:cmd(utils:concat([   "echo \"gnome-terminal -e 'erl -name ", 
    %                        Entity#newCarEntity.name, 
    %                        "@", 
    %                        Entity#newCarEntity.name, 
    %                        " -run car_moq start ",
    %                        Entity#newCarEntity.name, " ",
    %                        Entity#newCarEntity.side, " ", 
    %                        lists:flatten(io_lib:format("~p", [Entity#newCarEntity.power])), " ",
    %                        lists:flatten(io_lib:format("~p", [Entity#newCarEntity.timeout])),
    %                        "'\" > a.txt; gedit a.txt &"])).
    [Settings] = settings_repository:get_all(),
    os:cmd(utils:concat([   "cd ../../../../../car;gnome-terminal -e 'erl -name ", 
                            Entity#newCarEntity.name, 
                            "@", 
                            Entity#newCarEntity.name, 
                            " -run car start ",
                            Entity#newCarEntity.name, " ",
                            Entity#newCarEntity.side, " ", 
                            lists:flatten(io_lib:format("~p", [Entity#newCarEntity.power])), " ",
                            lists:flatten(io_lib:format("~p", [Settings#settingsEntity.turn])), " ", 
                            lists:flatten(io_lib:format("~p", [Settings#settingsEntity.bridgeCapacity])), " ", 
                            lists:flatten(io_lib:format("~p", [Settings#settingsEntity.bridgeCrossingTime])), " ", 
                            lists:flatten(io_lib:format("~p", [Entity#newCarEntity.timeout])),
                            "'"])).

%%%===================================================================
%%% private functions
%%%===================================================================