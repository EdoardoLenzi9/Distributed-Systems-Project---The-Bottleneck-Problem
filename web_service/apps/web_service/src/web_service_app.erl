%%%-------------------------------------------------------------------
%% @doc web_service public API
%% @end
%%%-------------------------------------------------------------------

-module(web_service_app).
-compile(export_all).
-behaviour(application).


start(_StartType, _StartArgs) ->
    {ok, Pid} = 'web_service_sup':start_link(),
    Routes = [ {
        '_', %% Host match https://ninenines.eu/docs/en/cowboy/1.0/guide/routing/
        [
            % Simulation end points
            {"/simulation", simulation_controller, []},   %% returns the current simulation state

            % Car end points
            {"/car/sync", car_controller, []},        %% creates a new car
            {"/car/adj", car_controller, []},        %% creates a new car

            % Client 
            {"/assets/[...]", cowboy_static, {dir, "../../../../client"}}
        ]
    } ],
    Dispatch = cowboy_router:compile(Routes),

    NumAcceptors = 100, %% max_keepalive https://ninenines.eu/docs/en/cowboy/1.0/guide/http_req_life/
    TransOpts = [ {ip, {0,0,0,0}}, {port, 8090} ],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],

    {ok, _} = cowboy:start_http(web_server,
        NumAcceptors, TransOpts, ProtoOpts),

    {ok, Pid}.

stop(_State) ->
    ok.


%% internal functions
