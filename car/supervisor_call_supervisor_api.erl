%%%===================================================================
%%% API for fsm to call its supervisor
%%%===================================================================


-module(supervisor_call_supervisor_api).
-compile(export_all).
-include("car.hrl").


%check(Sender, Target) ->
%    utils:log("send check"),
%    call_supervisor(Target#car_state.name, {sup_call_check, Sender, Target#car_state.name}).


sup_call(Req) ->
    utils:log("Supervisor call supervisor"),
    {_Label, _Sender, Target, _SendingTime, _Body} = Req,
    call_supervisor(Target, {sup_call, Req}).        


sup_response(Response) ->
    utils:log("Supervisor response supervisor"),
    {_Label, _Sender, Target, _SendingTime, _Body} = Response,
    call_supervisor(Target, {sup_response, Response}). 


call_supervisor(Name, Event) ->
    utils:log("send event"),
    {supervisor, list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Name))} ! Event.