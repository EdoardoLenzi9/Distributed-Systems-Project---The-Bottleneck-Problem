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
                default ->
                    {next_state, normal, Data, [{reply, From, sync}]}
            end.


normal({call, From}, Event, Data) ->
    utils:log("STATE Normal"),
    case Event of        
        default ->
            {keep_state,Data,[{reply,From,normal}]}
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