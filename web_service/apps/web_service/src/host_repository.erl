%% @author Edoardo Lenzi, Talissa Dreossi
%% @copyright GPL-3
%% @version 1.0.0


-module( host_repository ).
-compile( export_all ).
-include( "entity.hrl" ).


%%%===================================================================
%%% public functions
%%%===================================================================

add_range( Range ) ->
    db_manager:clear( host_entity ),
    add_range_wrapper( Range ).    


add_range_wrapper( [ ] )  ->
    ok;
add_range_wrapper( [ First | Rest ] ) ->
    add( First ),
    add_range_wrapper( Rest ).


add( Entity ) ->
    repository_helper:add( Entity ).


get_all() ->
    order( repository_helper:get_all( host_entity ) ).


reset() ->
    db_manager:start().


select( Car ) ->
    F = fun() -> 
        db_manager:select( host_entity, #host_entity{ 
                                                        host = atom_to_list( Car#adj_entity.host ), 
                                                        ip = atom_to_list( Car#adj_entity.ip ), 
                                                        _ = '_'
                                                    }, [ ], ['$_'] )
    end,
    { atomic, Res } = mnesia:transaction( F ),
    Res.


order( List ) ->
    F = fun( X, Y ) -> 
            X#host_entity.number_of_cars < Y#host_entity.number_of_cars
        end,
    lists:sort( F, List ).
