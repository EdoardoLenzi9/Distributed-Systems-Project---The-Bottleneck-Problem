-module(cowboy_hello_world_root).

-export([init/2]).

init(Req, Opts) ->
    io:format("~p", [cowboy_req:method(Req)]),
    Req2 = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/plain">>}],
        <<"Hello Erlang!">>,
        Req),
    {ok, Req2, Opts}.
