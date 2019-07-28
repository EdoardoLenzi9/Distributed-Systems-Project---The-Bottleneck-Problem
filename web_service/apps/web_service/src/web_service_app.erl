%% @author Edoardo Lenzi, Talissa Dreossi
%% @copyright GPL-3
%% @version 1.0.0


%%%-------------------------------------------------------------------
%% @doc web_service public API
%% @end
%%%-------------------------------------------------------------------

-module( web_service_app ).
-compile( export_all ).
-behaviour( application ).


start( _StartType, _StartArgs ) ->
    { ok, Pid } = 'web_service_sup':start_link(),
    Routes = [ {
        '_', %% Host match https://ninenines.eu/docs/en/cowboy/1.0/guide/routing/
        [
            % Simulation end points
            { "/simulation", simulation_controller, [ ] },         %% returns the current simulation state
            { "/simulation/init", simulation_controller, [ ] },    %% init simulation parameters
            { "/simulation/new", simulation_controller, [ ] },     %% launch new car node
            { "/simulation/reset", simulation_controller, [ ] },   %% reset simulation, kill every node

            % Car end points
            { "/car/sync", car_controller, [ ] },                  %% update car sync
            { "/car/adj", car_controller, [ ] },                   %% update car adj
            { "/car/adj/last", car_controller, [ ] },              %% get last car in queue
            { "/car/state", car_controller, [ ] },                 %% update car state
            { "/car/kill", car_controller, [ ] },                  %% kill target car

            % Client 
            { "/", cowboy_static, { file, "../../../../client/index.html" } },
            { "/[...]", cowboy_static, { dir, "../../../../client" } }
        ]
    } ],
    Dispatch = cowboy_router:compile( Routes ),

    NumAcceptors = 100, %% max_keepalive https://ninenines.eu/docs/en/cowboy/1.0/guide/http_req_life/
    TransOpts = [ { ip, { 0,0,0,0 } }, { port, 8090 } ],
    ProtoOpts = [ { env, [ { dispatch, Dispatch } ] } ],

    cowboy:start_http( web_server, NumAcceptors, TransOpts, ProtoOpts ),
    { ok, Pid }.

stop( _State ) ->
    ok.