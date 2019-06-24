%%%===================================================================
%%% API
%%%===================================================================

-compile(export_all).


bs() -> 
    start_link().


start_link(Timeout) ->
    gen_statem:start_link({global, ?MODULE}, ?MODULE, [Timeout], []).


start_link() ->
    gen_statem:start_link({global, ?MODULE}, ?MODULE, [], []).
 

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