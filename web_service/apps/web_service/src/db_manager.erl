-module(db_manager).
-compile(export_all).
-include("entity.hrl").


start() ->
    mnesia:start(),
    mnesia:create_schema([node()]), 
    create_table_scheme().
    %initStettings(Turn, BridgeCapacity, BridgeCrossingTime).


create_table_scheme() ->
    mnesia:create_table(settings, [{attributes, record_info(fields, settings)}]),
    mnesia:create_table(syncEntity, [{attributes, record_info(fields, syncEntity)}]),
    mnesia:create_table(adjEntity, [{attributes, record_info(fields, adjEntity)}]).


add(Item) ->
    Fun = fun() -> mnesia:write(Item) end,
    mnesia:transaction(Fun).


get_all(Entity) ->
    F = fun() -> mnesia:select(Entity,[{'_',[],['$_']}]) end,
    mnesia:transaction(F).


getTimeStamp() ->
    {Mega, Seconds, Ms} = os:timestamp(),
    (Mega*1000000 + Seconds)*1000 + erlang:round(Ms/1000).     