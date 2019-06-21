%%%-------------------------------------------------------------------
%% @doc cowboy_hello_world public API
%% @end
%%%-------------------------------------------------------------------

-module(cowboy_hello_world_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Pid} = 'cowboy_hello_world_sup':start_link(),
    
    %%To make Cowboy useful, you need to map URLs to Erlang modules that will handle the requests (route)
    %%Routes = [Host1, Host2, ... HostN].
    %%Host1 = {HostMatch, PathsList}.
    %%Host2 = {HostMatch, Constraints, PathsList}.
    %%PathsList = [Path1, Path2, ... PathN].
    %%Path1 = {PathMatch, Handler, Opts}.
    %%Path2 = {PathMatch, Constraints, Handler, Opts}.
    
    Routes = [ {
        '_', %% Host match https://ninenines.eu/docs/en/cowboy/1.0/guide/routing/
        [
            {"/", cowboy_hello_world_root, []}, %% path list
            {"/create/car", cowboy_hello_world_create, []}
        ]
    } ],
    Dispatch = cowboy_router:compile(Routes),

    NumAcceptors = 100, %% max_keepalive https://ninenines.eu/docs/en/cowboy/1.0/guide/http_req_life/
    TransOpts = [ {ip, {0,0,0,0}}, {port, 8180} ],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],

    {ok, _} = cowboy:start_http(chicken_poo_poo,
        NumAcceptors, TransOpts, ProtoOpts),

    {ok, Pid}.

stop(_State) ->
    ok.

%% internal functions
