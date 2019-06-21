
%% @doc car handler.
-module(car_controller).

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
    os:cmd("sh ../../../../../launcher/launcher.sh"),
	Body = <<"{\"rest\": \"Car created!\"}">>,
	{Body, Req, State}.