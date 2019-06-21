
%% @doc car handler.
-module(simulation_controller).

-export([init/2]).
-export([content_types_provided/2]).
-export([check/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTION">>], Req, State}.

content_types_provided(Req, State) ->
	{[
	    {<<"application/json">>, check}
	], Req, State}.

check(Req, State) ->
	Body = <<"{\"rest\": \"Simulation state!\"}">>,
	{Body, Req, State}.