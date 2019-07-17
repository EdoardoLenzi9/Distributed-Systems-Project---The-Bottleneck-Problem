-module(http_client).
-compile(export_all).
-include("car.hrl").


%%%===================================================================
%%% web service calls
%%%===================================================================

get_sync(Name, Side, Power, Host, Port) -> 
    Content = {[{name, Name}, {side, Side}, {power, Power}]},
    http_client:call(post, Host, Port, "/car/sync", Content, car, unmarshalling_sync).


get_adj(Data) -> 
    Host = Data#car_state.host,
    Port = Data#car_state.port,
    Content = {[    {name, Data#car_state.name}, 
                    {side, Data#car_state.side}, 
                    {power, Data#car_state.power},
                    {size, Data#car_state.size},
                    {position, Data#car_state.position},
                    {crossing, Data#car_state.crossing},
                    {arrival_time, Data#car_state.arrival_time},
                    {delta, Data#car_state.delta},
                    {state, Data#car_state.state},
                    {crash_type, Data#car_state.crash_type}    
            ]},
    http_client:call(post, Host, Port, "/car/adj", Content, car, unmarshalling_adj).


%%%===================================================================
%%% HTTP client
%%%===================================================================

call(Method, Host, Port, Uri, Content, Module, Unmarshalling) ->
    inets:start(),
    {ok, {_, _, Body}} = httpc:request(Method, {utils:concat(["http://", Host, ":", Port, Uri]), "application/json", "application/json", marshalling(Content)}, [], []),
    utils:log("Body: ~p~n~n", [Body]),
    A= jiffy:decode(Body),
    Module:Unmarshalling(A).


marshalling(Content) ->
    jiffy:encode(Content).

