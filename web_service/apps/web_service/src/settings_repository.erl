-module(settings_repository).
-compile(export_all).
-include("entity.hrl").

%%%===================================================================
%%% public functions
%%%===================================================================

add(Entity) ->
    db_manager:clear(settingsEntity),
    db_manager:add(Entity).


get_all() ->
    db_manager:get_all(settingsEntity).