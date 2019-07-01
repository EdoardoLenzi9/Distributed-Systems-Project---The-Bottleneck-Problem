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


check(Name, Check) ->
    utils:log("API Check"),
    gen_statem:call({global, Name}, {response_check, Check}).


defaultBehaviour(Name) ->
    utils:log("API defaultBehaviour"),
    gen_statem:call({global, Name}, defaultBehaviour).
% 
%
%crash(Name) ->
%    utils:log("API Crash"),
%    gen_statem:call({global, Name}, crash).
%
%
%newleader(Name) ->
%    utils:log("API New Leader"),
%    gen_statem:call({global, Name}, newleader).