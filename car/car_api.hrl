%%%===================================================================
%%% API
%%%===================================================================

-compile(export_all).


start(Args) -> 
    [PName, PSide, PPower, PTurn, PBridgeCapacity, PBridgeCrossingTime, PTimeout] = Args,
    Name = list_to_atom(PName),
    Side = list_to_atom(PSide),
    {Power, _ } = string:to_integer(PPower),
    {Turn, _ } = string:to_integer(PTurn),
    {BridgeCapacity, _ } = string:to_integer(PBridgeCapacity),
    {BridgeCrossingTime, _ } = string:to_integer(PBridgeCrossingTime),
	{Timeout, _ } = string:to_integer(PTimeout),
	if Timeout > 0 ->
		start_link(Name, Side, Power, Turn, BridgeCapacity, BridgeCrossingTime, Timeout);
	true ->
		start_link(Name, Side, Power, Turn, BridgeCapacity, BridgeCrossingTime)
	end.


bs() -> 
    start_link(car1, right, 1, 1000, 3, 3000).


start_link(Name, Side, Power, Turn, BridgeCapacity, BridgeCrossingTime, Timeout) ->
    utils:log("API Start link with timeout"),
    gen_statem:start_link({global, Name}, ?MODULE, [Name, Side, Power, Turn, BridgeCapacity, BridgeCrossingTime, Timeout], []).


start_link(Name, Side, Power, Turn, BridgeCapacity, BridgeCrossingTime) ->
    utils:log("API Start link"),
    gen_statem:start_link({global, Name}, ?MODULE, [Name, Side, Power, Turn, BridgeCapacity, BridgeCrossingTime], []).
 

stop(Name) ->
    utils:log("API Stop"),
    gen_statem:stop({global, Name}).


sync(Name) ->
    utils:log("API Sync"),
    gen_statem:call({global, Name}, sync).
 

crash(Name) ->
    utils:log("API Crash"),
    gen_statem:call({global, Name}, crash).


newleader(Name) ->
    utils:log("API New Leader"),
    gen_statem:call({global, Name}, newleader).