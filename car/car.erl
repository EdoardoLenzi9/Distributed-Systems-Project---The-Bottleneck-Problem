-module(car).
-behaviour(gen_statem).

-include("car.hrl"). 
-include("car_std.hrl").
-include("car_api.hrl").


%%%===================================================================
%%% gen_statem states
%%%===================================================================


init([State]) ->
    utils:log("STATE Init"),
    {ok, sync, State}.
        

sync({call, From}, Event, Data) ->
    utils:log("STATE Sync"),
    case Event of     
    default_behaviour ->
        utils:log("Event default_behaviour"),
        FrontCars = Data#carState.adj#adj.frontCars,
        [Pivot] = FrontCars,
        car_supervisor_api:check(Data, Pivot),
        flow:keep(Data, From, sync_default_behaviour);
    {response_check, Check} ->
        utils:log("Event response_check"),
        % berkeley
        CurrentTime = utils:get_timestamp(), 
        RTT = CurrentTime - Check#carState.sendingTime,
        PivotTime = Check#carState.currentTime,
        Delta = CurrentTime - (PivotTime + RTT / 2),
        flow:next(normal, update_delta(Data, Delta), From, {sync_response_check, Delta})
    end.


normal({call, From}, Event, Data) ->
    utils:log("STATE Normal"),
    case Event of        
        default_behaviour ->
            flow:keep(Data, From, normal_default_behaviour);
        dead ->
            flow:next(dead, Data, From, rip)
    end.


leader({call, From}, Event, Data) ->
    utils:log("STATE Queue"),
    case Event of
        default_behaviour ->
            flow:keep(From, Data)
        end.


dead({call, _From}, Event, Data) -> 
    utils:log("STATE Dead"),
    case Event of
        default_behaviour ->
            utils:log("Event dead_default_behaviour"),
            stop(Data#carState.name)
    end.