%%%===================================================================
%%% API for a supervisor to call/respond another supervisor
%%%===================================================================


-module(supervisor_reply_supervisor_api).
-compile(export_all).
-include("car.hrl").


sup_reply(Response) ->
    utils:log("Supervisor reply timer"),
    {_Label, _Sender, Target, Nickname, _SendingTime, _Body} = Response,
    reply_supervisor(Target, Nickname, {sup_reply, Response}). 


timer_reply(Response) ->
    utils:log("Timer reply supervisor"),
    {Label, Sender, Target, SendingTime, Body} = Response,
    reply_supervisor(Target, supervisor, {timer_reply, {Label, Sender, Target, SendingTime, Body}}). 


reply_supervisor(Name, Nickname, Event) ->
    utils:log("send event"),
    {Nickname, Name} ! Event.