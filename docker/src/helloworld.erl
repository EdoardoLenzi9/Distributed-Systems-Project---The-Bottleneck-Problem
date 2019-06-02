-module(helloworld).
-export([start/0, main/1]).

start() ->
    io:format("hello world", []),
    start().

main(args) ->
    start().