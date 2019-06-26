%%%===================================================================
%%% API
%%%===================================================================

-compile(export_all).


bs() -> 
    start_link(car1, "right", 2, 3, 3000).


% TODO update api
start_link(Name, Side, Power, BridgeCapacity, BridgeCrossingTime, Timeout) ->
    gen_statem:start_link({global, Name}, ?MODULE, [Name, Side, Power, BridgeCapacity, BridgeCrossingTime, Timeout], []).


start_link(Name, Side, Power, BridgeCapacity, BridgeCrossingTime) ->
    gen_statem:start_link({global, Name}, ?MODULE, [Name, Side, Power, BridgeCapacity, BridgeCrossingTime], []).
 

stop(Name) ->
        gen_statem:stop({global, Name}).


sync(Name) ->
        gen_statem:call({global, Name}, sync).
 

crash(Name) ->
        gen_statem:call({global, Name}, crash).


newleader(Name) ->
        gen_statem:call({global, Name}, newleader).