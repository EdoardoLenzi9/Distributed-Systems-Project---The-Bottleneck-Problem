-module(db_manager).
-compile(export_all).
-include("entity.hrl").


start() ->
    mnesia:start(),
    mnesia:create_schema([node()]), 
    create_table_scheme(),
    clear_all(),
    settings_repository:add(utils:load_environment()).


create_table_scheme() ->
    mnesia:create_table(settingsEntity, [{attributes, record_info(fields, settingsEntity)}]),
    mnesia:create_table(sync_entity, [{attributes, record_info(fields, sync_entity)}]),
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
    clear(settingsEntity).


add(Item) ->
    Fun = fun() -> mnesia:write(Item) end,
    mnesia:transaction(Fun).


delete(Item) ->
    Fun = fun() -> mnesia:delete_object(Item) end,
    mnesia:transaction(Fun).


get_all(Entity) ->
    F = fun() -> mnesia:select(Entity,[{'_',[],['$_']}]) end,
    {atomic, Data} = mnesia:transaction(F),
    Data.


select(Entity, MatchHead, Guard, Result) ->
    F = fun() -> mnesia:select(Entity,[{MatchHead, Guard, Result}]) end,
    {atomic, Data} = mnesia:transaction(F),
    Data.