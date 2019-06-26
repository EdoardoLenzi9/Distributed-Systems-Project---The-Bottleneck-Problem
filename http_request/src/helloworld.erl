-module(helloworld).
-export([start/0, main/1]).

start() ->
    io:format("hello world", []),
    inets:start(),
    httpc:request(post, {"http://localhost:8090/car/sync", "application/json", "application/json", "{
        \"name\": \"car123\",
        \"side\": \"left\",
        \"power\": 2
    }"}, [], []).
main(args) ->
    start().
