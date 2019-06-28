-module(http_client).
-compile(export_all).
-include("car.hrl").
% TODO load url from environment.json
-define(URL, "http://localhost:8090").


%%%===================================================================
%%% web service calls
%%%===================================================================

getSyncAdj(Name, Side, Power) -> 
    Content = {[{name, Name}, {side, Side}, {power, Power}]},
    http_client:call(post, "/car/sync", Content, car, unmarshalling_sync).


getAdj(Data) -> 
    Content = {[    {name, Data#carState.name}, 
                    {side, Data#carState.side}, 
                    {power, Data#carState.power},
                    {arrivalTime, Data#carState.arrivalTime},
                    {delta, Data#carState.delta},
                    {state, Data#carState.state}    
            ]},
    http_client:call(post, "/car/adj", Content, car, unmarshalling_adj).


%%%===================================================================
%%% HTTP client
%%%===================================================================

call(Method, Uri, Content, Module, Unmarshalling) ->
    inets:start(),
    {ok, {_, _, Body}} = httpc:request(Method, {?URL ++ Uri, "application/json", "application/json", marshalling(Content)}, [], []),
    Module:Unmarshalling(jiffy:decode(Body)).


marshalling(Content) ->
    jiffy:encode(Content).

