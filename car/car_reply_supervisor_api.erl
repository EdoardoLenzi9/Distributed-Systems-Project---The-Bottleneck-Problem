%%%===================================================================
%%% API for fsm to respond to its supervisor's call
%%%===================================================================


-module(car_reply_supervisor_api).
-compile(export_all).
-include("car.hrl").


car_reply(Response) ->
    {_Label, _Sender, Target, Nickname, _SendingTime, _Body} = Response, 
    call_supervisor(Target, {car_reply, Response}).        


call_supervisor(Name, Event) ->
    utils:log("send car reply"),
    {supervisor, list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Name))} ! Event.