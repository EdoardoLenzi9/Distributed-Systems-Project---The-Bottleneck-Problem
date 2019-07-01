-module(load_json).
-compile(export_all).

read() ->
    {ok, Content} = file:read_file("environment.json"),
    jiffy:decode(Content).