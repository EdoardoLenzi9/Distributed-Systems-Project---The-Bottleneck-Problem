-module(http_client).
-compile(export_all).
-include("car.hrl").
% TODO load url from environment.json
-define(URL, "http://localhost:8090").


%%%===================================================================
%%% web service calls
%%%===================================================================

get_sync(Name, Side, Power) -> 
    Content = {[{name, Name}, {side, Side}, {power, Power}]},
    http_client:call(post, "/car/sync", Content, car, unmarshalling_sync).


get_adj(Data) -> 
    Content = {[    {name, Data#car_state.name}, 
                    {side, Data#car_state.side}, 
                    {power, Data#car_state.power},
                    {arrival_time, Data#car_state.arrival_time},
                    {delta, Data#car_state.delta},
                    {state, Data#car_state.state}    
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

