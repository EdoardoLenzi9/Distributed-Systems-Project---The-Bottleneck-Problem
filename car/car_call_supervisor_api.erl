%%%===================================================================
%%% API for fsm to call its supervisor
%%%===================================================================


-module(car_call_supervisor_api).
-compile(export_all).
-include("car.hrl").


car_call(Req) ->
    utils:log("Car call supervisor"),
    {_Label, Sender, _Target, _Body} = Req,
    call_supervisor(Sender, {car_call, Req}).


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