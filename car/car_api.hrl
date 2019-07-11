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
    
tow_truck(Name) ->
    utils:log("API tow_truck"),
    gen_statem:call({global, Name}, tow_truck).


default_behaviour(Name) ->
    utils:log("API default_behaviour"),
    gen_statem:call({global, Name}, default_behaviour).

    
crash(Name) ->
    utils:log("API Crash"),
    gen_statem:call({global, Name}, crash).


adj_reply(Name, Adj) ->
    utils:log("API Adj Reply"),
    gen_statem:call({global, Name}, {adj_reply, Adj}).


update(Name, Replacement) ->
    utils:log("API Update"),
    gen_statem:call({global, Name}, {update, Replacement}).


timeout(Name, Target) ->
    utils:log("API Timeout Reply"),
    gen_statem:call({global, Name}, {timeout, Target}).


% Req/reply events

check(Req) ->
    utils:log("API Check Request"),
    {_Label, _Sender, Target, _Nickname, _SendingTime, _Body} = Req,
    gen_statem:call({global, Target}, {check, Req}).


check_reply(Reply) ->
    utils:log("API Check Reply"),
    {_Sender, Target, _SendingTime, _RTT, _Body} = Reply,
    gen_statem:call({global, Target}, {check_reply, Reply}).


crossing(Req) ->
    utils:log("API Crossing Request"),
    {_Label, _Sender, Target, _SendingTime, _Body} = Req,
    gen_statem:call({global, Target}, {crossing, Req}).
        
        
crossing_reply(Reply) ->
    utils:log("API Crossing Reply"),
    {_Label, _Sender, Target, _SendingTime, _RTT, _Body} = Reply,
    gen_statem:call({global, Target}, {crossing_reply, Reply}).


