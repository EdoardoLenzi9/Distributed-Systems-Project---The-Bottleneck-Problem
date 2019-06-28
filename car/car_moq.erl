-module(car_moq).
-compile(export_all).

start(Args) -> 
    [Name, Side, Power, Timeout] = Args,
    {Power2, _ } = string:to_integer(Power),
    {Timeout2, _ } = string:to_integer(Timeout),
    timer:apply_after(1000, io, format, ["args ~p~n", [Args]]),
    timer:apply_after(1000, io, format, ["name: ~p~n", [list_to_atom(Name)]]),
    timer:apply_after(1000, io, format, ["side: ~p~n", [list_to_atom(Side)]]),
    timer:apply_after(1000, io, format, ["power: ~p~n", [Power2]]),
    timer:apply_after(1000, io, format, ["timeout: ~p~n", [Timeout2]]).