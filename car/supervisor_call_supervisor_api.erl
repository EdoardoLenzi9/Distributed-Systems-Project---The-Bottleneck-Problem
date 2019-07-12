%%%===================================================================
%%% API for a supervisor to call/respond another supervisor
%%%===================================================================


-module(supervisor_call_supervisor_api).
-compile(export_all).
-include("car.hrl").


timer_call(Req) ->
    utils:log("Timer call supervisor"),
    {_Label, _Sender, Target, _Nickname, _SendingTime, _Body} = Req,
    call_supervisor(Target, {timer_call, Req}). 


call_supervisor(Name, Event) ->
    utils:log("send event"),
    {supervisor, Name} ! Event.