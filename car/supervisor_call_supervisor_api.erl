%%%===================================================================
%%% API for a supervisor to call/respond another supervisor
%%%===================================================================


-module(supervisor_call_supervisor_api).
-compile(export_all).
-include("car.hrl").


sup_call(Req) ->
    utils:log("Supervisor call supervisor"),
    {_Label, _Sender, Target, _Nickname, _SendingTime, _Body} = Req,
    call_supervisor(Target, {sup_call, Req}).        


timer_call(Req) ->
    utils:log("Timer call supervisor"),
    {_Label, _Sender, Target, _Nickname, _SendingTime, _Body} = Req,
    call_supervisor(Target, {timer_call, Req}). 


call_supervisor(Name, Event) ->
    utils:log("send event"),
    {supervisor, list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Name))} ! Event.