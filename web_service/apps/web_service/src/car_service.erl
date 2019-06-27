-module(car_service).
-compile(export_all).
-include("entity.hrl").

add_sync(Name, Side, Power) ->
    Sync = if Side == "left" -> 
        sync_repository:get_first(Power);
    Side == "right" ->
        lists:reverse(sync_repository:get_last(Power))
    end,    
    sync_repository:add(Name, Side, Power),
    sync_marshalling(Sync).


add_adj(Name, Side, Power, ArrivalTime, Delta, State) ->
    [Front, Rear] = if Side == "left" -> 
        adj_repository:get_left(Name, Power);
    Side == "right" ->
        adj_repository:get_right(Name, Power)
    end,    
    adj_repository:add(Name, Side, Power, ArrivalTime, Delta, State),
    [adj_marshalling(Front), adj_marshalling(Rear)].
