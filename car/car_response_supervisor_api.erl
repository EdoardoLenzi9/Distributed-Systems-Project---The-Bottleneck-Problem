%%%===================================================================
%%% API for fsm to call its supervisor
%%%===================================================================


-module(car_response_supervisor_api).
-compile(export_all).
-include("car.hrl").


car_response(Response) ->
    {_Label, _Sender, Target, _SendingTime, _Body} = Response, 
    call_supervisor(Target, {car_response, Response}).        


%check(State, Target) ->
%    utils:log("send check"),
%    call_supervisor(State#car_state.name, {check, State#car_state.name, Data#car_state{sending_time = Target}}).
%    
%    
%adj(State) ->
%    utils:log("send adj"),
%    call_supervisor(State#car_state.name, {adj, State#car_state.name, State}).


call_supervisor(Name, Event) ->
    utils:log("send event"),
    {supervisor, list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Name))} ! Event.