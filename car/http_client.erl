%% @author Edoardo Lenzi, Talissa Dreossi
%% @copyright GPL-3
%% @version 1.0.0


%%%===================================================================
%%% web service calls utility
%%%===================================================================


-module( http_client ).
-compile( export_all ).
-include( "car.hrl" ).


%% @doc calls /car/sync end point

get_sync( Name, Side, Power, Host, Port ) -> 
    Content = { [ { name, Name }, { side, Side }, { power, Power } ] },
    call( post, Host, Port, "/car/sync", Content, http_client, unmarshalling_sync ).


%% @doc calls /car/adj end point

get_adj( Data ) -> 
    Host = Data#car_state.ws_host,
    Port = Data#car_state.ws_port,
    Content = { [    
                    { host, Data#car_state.host }, 
                    { ip, Data#car_state.ip }, 
                    { name, Data#car_state.name }, 
                    { side, Data#car_state.side }, 
                    { power, Data#car_state.power },
                    { size, Data#car_state.size },
                    { position, Data#car_state.position },
                    { crossing, Data#car_state.crossing },
                    { arrival_time, Data#car_state.arrival_time },
                    { delta, Data#car_state.delta },
                    { state, Data#car_state.state },
                    { crash_type, Data#car_state.crash_type }    
            ] },
    http_client:call( post, Host, Port, "/car/adj", Content, http_client, unmarshalling_adj ).


%% @doc calls /car/adj/last end point

get_last_adj( Data ) -> 
    Host = Data#car_state.ws_host,
    Port = Data#car_state.ws_port,
    Content = { [ {  side, Data#car_state.side } ] },
    http_client:call( post, Host, Port, "/car/adj/last", Content, http_client, unmarshalling_last_adj ).


%% @doc calls /car/kill end point

kill( Data, Name ) -> 
    Host = Data#car_state.ws_host,
    Port = Data#car_state.ws_port,
    Content = { [ 
                    {  name, Data#car_state.name }, 
                    {  target, Name } 
              ] },
    http_client:call( post, Host, Port, "/car/kill", Content, http_client, unmarshalling_kill ).
    

%%%===================================================================
%%% HTTP client
%%%===================================================================


%% @doc HTTP call primitive 

call( Method, Host, Port, Uri, Content, Module, Unmarshalling ) ->
    inets:start(),
    { ok, { _, _, Body } } = httpc:request( 
                                            Method, 
                                            { 
                                                utils:concat( [ "http://", Host, ":", Port, Uri ] ), 
                                                "application/json", 
                                                "application/json", 
                                                marshalling( Content ) 
                                            }, [ ], [ ] ),
    utils:log( "Body: ~p~n~n", [ Body ] ),
    Module:Unmarshalling( jiffy:decode( Body ) ).


%% @doc marshalling to JSON format 

marshalling( Content ) ->
    jiffy:encode( Content ).

