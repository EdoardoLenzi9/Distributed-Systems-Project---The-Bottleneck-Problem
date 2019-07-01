%%%===================================================================
%%% API for fsm to call its supervisor
%%%===================================================================


-module(car_supervisor_api).
-compile(export_all).
-include("car.hrl").


check(State, Target) ->
    utils:log("send check"),
    send_event(State#carState.name, {check, State#carState.name, update_sending_time(Target)}).


send_event(Name, Event) ->
    utils:log("send event"),
    {supervisor, list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Name))} ! Event.