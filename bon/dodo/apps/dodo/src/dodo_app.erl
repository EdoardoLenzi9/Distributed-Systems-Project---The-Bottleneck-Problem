%% @private
-module(dodo_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", toppage_h, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8081}], #{
		env => #{dispatch => Dispatch}
	}),
	dodo_sup:start_link().

stop(_State) ->
	ok.
