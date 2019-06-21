-module(helloworld).
-export([start/0, main/1]).

start() ->
    io:format("hello world", []),
    inets:start(),
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = httpc:request(get, {"http://localhost:8086/assets/index.html", []}, [], []).

main(args) ->
    start().