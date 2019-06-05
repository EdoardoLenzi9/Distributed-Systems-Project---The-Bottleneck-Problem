-module(launcher).
-export([start/0]).

start() ->
    "erl -run launcher start",
    os:cmd("sh launcher.sh").
