%%%===================================================================
%%% API
%%%===================================================================

-compile(export_all).


start_link(Name, State) ->
    utils:log("API Start link"),
    gen_statem:start({global, Name}, ?MODULE, [State], []).
 

stop(Name) ->
    utils:log("API Stop"),
    gen_statem:stop({global, Name}).

    
default_behaviour(Name) ->
    utils:log("API default_behaviour"),
    gen_statem:call({global, Name}, default_behaviour).

    
crash(Name) ->
    utils:log("API Crash"),
    gen_statem:call({global, Name}, dead).


check(Name, Check) ->
    utils:log("API Check"),
    gen_statem:call({global, Name}, {response_check, Check}).


% TODO update adiacenze
% TODO propagazione all'indietro del messaggio del leader