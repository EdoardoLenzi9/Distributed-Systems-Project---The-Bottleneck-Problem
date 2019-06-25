%%%===================================================================
%%% API
%%%===================================================================

-compile(export_all).


bs() -> 
    start_link(2).


start_link(bridgeCapacity, Timeout) ->
    gen_statem:start_link({global, ?MODULE}, ?MODULE, [bridgeCapacity, Timeout], []).


start_link(bridgeCapacity) ->
    gen_statem:start_link({global, ?MODULE}, ?MODULE, [bridgeCapacity], []).
 

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