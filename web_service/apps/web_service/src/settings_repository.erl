-module(settings_repository).
-compile(export_all).
-include("entity.hrl").

%%%===================================================================
%%% public functions
%%%===================================================================

add(Entity) ->
    db_manager:clear(settings_entity),
    repository_helper:add(Entity).


get_all() ->
    repository_helper:get_all(settings_entity).


reset() ->
    db_manager:start().