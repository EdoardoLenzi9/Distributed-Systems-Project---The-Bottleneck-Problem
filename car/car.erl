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
        utils:log("Call supervisor"),
        %Target = updateSendingTime(Pivot),
        %{supervisor, car1@car1} ! {check, Data#carState.name, updateSendingTime(Pivot)},
        car_supervisor_api:check(Data, Pivot),
        %utils:log("After call"),
        {keep_state, Data, [{reply, From, syncdefbehaviour}]};
    {response_check, Check} ->
        utils:log("Event response_check"),
        % berkeley
        CurrentTime = utils:getTimeStamp(), 
        RTT = CurrentTime - Check#carState.sendingTime,
        PivotTime = Check#carState.currentTime,
        Delta = CurrentTime - (PivotTime + RTT / 2),
        {next_state, normal, Data, [{reply, From, syncresponsebehaviour}]}
    end.


normal({call, From}, Event, Data) ->
    utils:log("STATE Normal"),
    case Event of        
        defaultBehaviour ->
            {keep_state, Data, [{reply, From, normaldefBehaviour}]}
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