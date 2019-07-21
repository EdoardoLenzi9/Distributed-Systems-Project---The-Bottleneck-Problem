-module(repository_helper).
-compile(export_all).


add(Item) ->
    Fun = fun() -> db_manager:add(Item) end,
    mnesia:transaction(Fun).


delete(Item) ->
    Fun = fun() -> db_manager:delete(Item) end,
    mnesia:transaction(Fun).


select(Entity, MatchHead, Guard, Result) ->
    F = fun() -> db_manager:select(Entity,[{MatchHead, Guard, Result}]) end,
    {atomic, Data} = mnesia:transaction(F),
    Data.


get_all(Entity) ->
    F = fun() -> db_manager:get_all(Entity) end,
    {atomic, Data} = mnesia:transaction(F),
    Data.