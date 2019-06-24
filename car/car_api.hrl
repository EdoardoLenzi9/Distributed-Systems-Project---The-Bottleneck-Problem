%%%===================================================================
%%% API
%%%===================================================================

-compile(export_all).


bs() -> 
    start_link(2).


start_link(BridgeLength, Timeout) ->
    gen_statem:start_link({global, ?MODULE}, ?MODULE, [BridgeLength, Timeout], []).


start_link(BridgeLength) ->
    gen_statem:start_link({global, ?MODULE}, ?MODULE, [BridgeLength], []).
 

stop() ->
        gen_statem:stop({global, ?MODULE}).


sync() ->
        gen_statem:call({global, ?MODULE}, sync).
 

crash() ->
        gen_statem:call({global, ?MODULE}, crash).


move() ->
        gen_statem:call({global, ?MODULE}, move).


newleader() ->
        gen_statem:call({global, ?MODULE}, newleader).