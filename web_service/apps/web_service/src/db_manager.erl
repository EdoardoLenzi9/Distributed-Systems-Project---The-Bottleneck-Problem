-module(db_manager).
-compile(export_all).
-include("entity.hrl").


start() ->
    mnesia:start(),
    mnesia:create_schema([node()]), 
    create_table_scheme(),
    clear_all(),
    settings_repository:add(#settingsEntity{turn = 1000, bridgeCapacity = 2, bridgeCrossingTime = 2000}).
    %initStettings(Turn, BridgeCapacity, BridgeCrossingTime).


create_table_scheme() ->
    mnesia:create_table(settingsEntity, [{attributes, record_info(fields, settingsEntity)}]),
    mnesia:create_table(syncEntity, [{attributes, record_info(fields, syncEntity)}]),
    mnesia:create_table(adjEntity, [{attributes, record_info(fields, adjEntity)}]).


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
    clear(adjEntity),
    clear(syncEntity),
    clear(settingsEntity).


add(Item) ->
    Fun = fun() -> mnesia:write(Item) end,
    mnesia:transaction(Fun).


get_all(Entity) ->
    F = fun() -> mnesia:select(Entity,[{'_',[],['$_']}]) end,
    {atomic, Data} = mnesia:transaction(F),
    Data.
