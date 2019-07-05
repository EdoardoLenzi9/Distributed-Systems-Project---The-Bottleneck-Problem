%%%===================================================================
%%% API for fsm to respond to its supervisor's call
%%%===================================================================


-module(car_response_supervisor_api).
-compile(export_all).
-include("car.hrl").


car_response(Response) ->
    {_Label, _Sender, Target, _SendingTime, _Body} = Response, 
    call_supervisor(Target, {car_response, Response}).        


call_supervisor(Name, Event) ->
    utils:log("send event"),
    {supervisor, list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Name))} ! Event.