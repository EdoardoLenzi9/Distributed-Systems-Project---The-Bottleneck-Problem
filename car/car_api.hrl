%%%===================================================================
%%% API for supervisor to call its car
%%%===================================================================

-compile(export_all).


start_link(Name, State) ->
    utils:log("API Start link"),
    gen_statem:start({global, Name}, ?MODULE, [State], []).
 

stop(Name) ->
    utils:log("API Stop"),
    gen_statem:stop({global, Name}).


% Simple events
    
default_behaviour(Name) ->
    utils:log("API default_behaviour"),
    gen_statem:call({global, Name}, default_behaviour).

    
crash(Name) ->
    utils:log("API Crash"),
    gen_statem:call({global, Name}, crash).


adj_reply(Name, Adj) ->
    utils:log("API Adj Response"),
    gen_statem:call({global, Name}, {adj_reply, Adj}).


% Req/reply events

check(Req) ->
    utils:log("API Check Request"),
    {_Label, _Sender, Target, _Nickname, _SendingTime, _Body} = Req,
    gen_statem:call({global, Target}, {check, Req}).


check_reply(Response) ->
    utils:log("API Check Response"),
    {_Sender, Target, _SendingTime, _RTT, _Body} = Response,
    gen_statem:call({global, Target}, {check_reply, Response}).


crossing(Req) ->
    utils:log("API Crossing Request"),
    {_Label, _Sender, Target, _SendingTime, _Body} = Req,
    gen_statem:call({global, Target}, {crossing, Req}).
        
        
crossing_reply(Response) ->
    utils:log("API Crossing Response"),
    {_Label, _Sender, Target, _SendingTime, _RTT, _Body} = Response,
    gen_statem:call({global, Target}, {crossing_reply, Response}).
