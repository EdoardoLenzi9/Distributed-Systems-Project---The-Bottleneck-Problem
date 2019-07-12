%%%===================================================================
%%% API for fsm to call its supervisor
%%%===================================================================


-module(car_call_supervisor_api).
-compile(export_all).
-include("car.hrl").


car_call(Req) ->
    utils:log("Car call supervisor ~p", [Req]),
    {_Label, Sender, _Target, _RTT, _Body} = Req,
    call_supervisor(Sender, {car_call, Req}).


call_supervisor(Name, Event) ->
    utils:log("send event"),
    {supervisor, list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Name))} ! Event.