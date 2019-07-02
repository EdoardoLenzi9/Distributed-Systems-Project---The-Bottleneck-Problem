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
    {check, Req} ->     
        utils:log("Event check"),
        {_Label, Sender, _Target, SendingTime, _Body} = Req,
        car_response_supervisor_api:car_response({check_response, Data#car_state.name, Sender, SendingTime, Data#car_state{current_time = utils:get_timestamp()}}),
        flow:keep(Data, From, {sync_check, Data});
    {response_check, Response} ->
        utils:log("Event response_check"),
        % berkeley
        {_Label, _Sender, _Target, SendingTime, RTT, Body} = Response,
        CurrentTime = SendingTime, 
        PivotTime = Body#car_state.current_time,
        Delta = CurrentTime - (PivotTime + RTT / 2),
        %Adj = 
        flow:next(normal, Data#car_state{delta = Delta}, From, {normal, Data#car_state{delta = Delta}});
    default_behaviour ->
        utils:log("Event default_behaviour"),
        FrontCars = Data#car_state.adj#adj.front_cars,
        if length(FrontCars) > 0 ->
            [Pivot] = FrontCars,
            utils:log("Start call"),
            car_call_supervisor_api:car_call({check, Data#car_state.name, Pivot#car_state.name, {}}),
            flow:keep(Data, From, {sync_default_behaviour, Data});
        true ->
            flow:next(normal, Data, From, {normal, Data})
        end
    end.


normal({call, From}, Event, Data) ->
    utils:log("STATE Normal"),
    case Event of        
        default_behaviour ->
            flow:keep(Data, From, {normal_default_behaviour, Data})
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
            utils:log("Event default_behaviour"),
            stop(Data#car_state.name)
    end.