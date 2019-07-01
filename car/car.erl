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
    defaultBehaviour ->
        utils:log("Event defaultBehaviour"),
        FrontCars = Data#carState.adj#adj.frontCars,
        [Pivot] = FrontCars,
        car_supervisor_api:check(Data, Pivot),
        flow:keep(Data, From, sync_default_behaviour);
    {response_check, Check} ->
        utils:log("Event response_check"),
        % berkeley
        CurrentTime = utils:getTimeStamp(), 
        RTT = CurrentTime - Check#carState.sendingTime,
        PivotTime = Check#carState.currentTime,
        Delta = CurrentTime - (PivotTime + RTT / 2),
        flow:next(normal, updateDelta(Data, Delta), From, {sync_response_check, Delta})
    end.


normal({call, From}, Event, Data) ->
    utils:log("STATE Normal"),
    case Event of        
        defaultBehaviour ->
            flow:keep(Data, From, normaldefBehaviour)
    end.


leader({call, From}, Event, Data) ->
    utils:log("STATE Queue"),
    case Event of
        defaultBehaviour ->
            flow:keep(From, Data)
        end.


dead({call, _From}, Event, Data) -> 
    utils:log("STATE Dead"),
    case Event of
        defaultBehaviour ->
            utils:log("Event defaultBehaviour"),
            stop(Data#carState.name)
    end.