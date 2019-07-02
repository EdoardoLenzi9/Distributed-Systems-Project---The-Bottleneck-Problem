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
    gen_statem:call({global, Name}, crash).


check(Req) ->
    utils:log("API Check Request"),
    {_Label, _Sender, Target, _SendingTime, _Body} = Req,
    gen_statem:call({global, Target}, {check, Req}).


check_response(Response) ->
    utils:log("API Check Response"),
    {_Label, _Sender, Target, _SendingTime, _RTT, _Body} = Response,
    gen_statem:call({global, Target}, {response_check, Response}).

% TODO update adiacenze
% TODO propagazione all'indietro del messaggio del leader