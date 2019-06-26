-module(http_client).
-compile(export_all).
-define(URL, "http://localhost:8090").

call(Method, Uri, Content, Module, Unmarshalling) ->
    inets:start(),
    %httpc:request(post, {"http://localhost:8090/car/sync", "application/json", "application/json", "{
    %    \"name\": \"car123\",
    %    \"side\": \"left\",
    %    \"power\": 2
    %}"}, [], []).
    {ok, {_, _, Body}} = httpc:request(Method, {?URL ++ Uri, "application/json", "application/json", marshalling(Content)}, [], []),
    Module:Unmarshalling(jiffy:decode(Body)).


marshalling(Content) ->
    jiffy:encode(Content).

