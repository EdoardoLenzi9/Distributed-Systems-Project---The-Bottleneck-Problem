-module(car_supervisor_api).
-compile(export_all).
-include("car.hrl").


check(State, Target) ->
    utils:log("send check"),
    sendEvent(State#carState.name, {check, State#carState.name, updateSendingTime(Target)}).


sendEvent(Name, Event) ->
    utils:log("send event"),
    {supervisor, list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Name))} ! Event.