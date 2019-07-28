-module(db_manager).
-compile(export_all).
-include("entity.hrl").


start() ->
    mnesia:start(),
    utils:log("Schema creation"),
    mnesia:create_schema([node()]), 
    create_table_scheme(),
    utils:log("Clear DB"),
    clear_all(),
    utils:log("Load environment"),
    settings_repository:add(utils:load_environment()),
    host_repository:add_range(utils:load_credentials()),
    utils:log("DB setup terminated").


create_table_scheme() ->
    mnesia:create_table(settings_entity, [{attributes, record_info(fields, settings_entity)}]),
    mnesia:create_table(sync_entity, [{attributes, record_info(fields, sync_entity)}]),
    mnesia:create_table(host_entity, [{attributes, record_info(fields, host_entity)}]),
    mnesia:create_table(adj_entity, [{attributes, record_info(fields, adj_entity)}]).
    

addRange(List) ->
    if length(List) > 0 -> 
        Fun = fun() ->
            addRangeWrapper(List)
    	end,
        mnesia:transaction(Fun);
    true ->
        ok
    end.    


addRangeWrapper([First | Rest]) ->
    mnesia:write(First),
    addRangeWrapper(Rest);
addRangeWrapper([]) ->
    ok.


clear(Entity) ->
    mnesia:clear_table(Entity).


clear_all() ->
    clear(adj_entity),
    clear(sync_entity),
    clear(host_entity),
    clear(settings_entity).


add(Item) ->
    mnesia:write(Item).


delete(Item) ->
    mnesia:delete_object(Item).


select(Entity, MatchHead, Guard, Result) ->
    mnesia:select(Entity,[{MatchHead, Guard, Result}]).


get_all(Entity) ->
    select(Entity, '_', [], ['$_']).


counter(Entity) ->
    length(get_all(Entity)).