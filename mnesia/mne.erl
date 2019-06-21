-module(mne).
-export([start/0]).

-record(car, {  name,
                state,
                direction,
                power,
                timer}).

start() ->
    "Initiate a new schema with an attribute that specifies on which node, or nodes, the database will operate.
    you have to specify at compile time (maybe possible to expand with an update command)",
    mnesia:create_schema([node()]), 
    mnesia:start(),
    create_table_schema(),
    create_record(#car{name="car2", state=safe, direction=right, power = 1, timer = none}),
    "multiple inserts, tables definitions,... are ignored",
    retrieve_record().

create_table_schema() ->
    mnesia:create_table(car, [{attributes, record_info(fields, car)}]).

create_record(Car) ->
    Fun = fun() -> mnesia:write(Car) end,
    mnesia:transaction(Fun).

retrieve_record() ->
    F = fun() -> mnesia:select(car,[{'_',[],['$_']}]) end,
    mnesia:transaction(F).