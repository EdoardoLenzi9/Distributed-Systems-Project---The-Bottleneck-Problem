
%% @doc car handler.
-module(simulation_controller).
-compile(export_all).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.


allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.


content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, handler}],
		Req, State}.


content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, handler}
	], Req, State}.


%%%===================================================================
%%% end-points
%%%===================================================================

handler(Req, State) ->
	URL = cowboy_req:url(Req),
	Method = cowboy_req:method(Req),
	{ok, Body, _Req2} = cowboy_req:body(Req),
	{_HTTP, _Domain, Path, _, _Qs} = mochiweb_util:urlsplit(binary_to_list(URL)),
	utils:log("~n~n~p    ~p    ~p~n~n", [Method, Path, Body]),
	ResponseBody = case Path of 
		"/simulation" ->
			state_handler(Body);
		"/simulation/init" ->
			init_handler(Body);
		"/simulation/new" ->
			new_node_handler(Body);
		"/simulation/reset" ->
			reset_handler(Body)
	end,
	Req3 = cowboy_req:set_resp_body(ResponseBody, Req),
	{true, Req3, State}.


state_handler(Body) ->
	ok.


init_handler(Body) ->
	ok.


new_node_handler(Body) ->
	ok.


reset_handler(Body) ->
	ok.