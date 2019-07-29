%% @author Edoardo Lenzi, Talissa Dreossi
%% @copyright GPL-3
%% @version 1.0.0


-module( adj_repository ).
-compile( export_all ).
-include( "entity.hrl" ).


%%%===================================================================
%%% public functions
%%%===================================================================

add( Entity ) ->
    repository_helper:add( Entity ).


delete( Entity ) ->
    repository_helper:delete( Entity ).


get_all() ->
    order( repository_helper:get_all( adj_entity ) ).


select( Name ) ->
    F = fun() -> 
        SelectResult = db_manager:select( adj_entity, #adj_entity{ name = Name, _ = '_' }, [ ], ['$_'] ),
        if length( SelectResult ) == 1 ->
            SelectResult;
        true ->
            [ ]
        end
    end,
    { atomic, Res } = mnesia:transaction( F ),
    Res.
%%%===================================================================
%%% private functions
%%%===================================================================

order( List ) ->
    F = fun( X, Y ) -> 
        if X#adj_entity.side == 1, Y#adj_entity.side == 1 -> 
            X#adj_entity.arrival_time + X#adj_entity.delta < Y#adj_entity.arrival_time + Y#adj_entity.delta;
        X#adj_entity.side == 1, Y#adj_entity.side == -1 ->
            false;
        X#adj_entity.side == -1, Y#adj_entity.side == 1 ->
            true; 
        X#adj_entity.side == -1, Y#adj_entity.side == -1 -> 
            X#adj_entity.arrival_time + X#adj_entity.delta > Y#adj_entity.arrival_time + Y#adj_entity.delta
            end
        end,
    lists:sort( F, List ).
