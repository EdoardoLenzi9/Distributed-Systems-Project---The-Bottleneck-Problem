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

    
crash(Name, CrashType) ->
    utils:log("API Crash"),
    gen_statem:call({global, Name}, {crash, CrashType}).


adj_reply(Name, Adj) ->
    utils:log("API Adj Reply"),
    gen_statem:call({global, Name}, {adj_reply, Adj}).


update_front(Name, Replacement) ->
    utils:log("API Update front"),
    gen_statem:call({global, Name}, {update_front, Replacement}).


update_rear(Name, Replacement) ->
    utils:log("API Update rear"),
    gen_statem:call({global, Name}, {update_rear, Replacement}).


timeout(Name, Target) ->
    utils:log("API Timeout Reply"),
    gen_statem:call({global, Name}, {timeout, Target}).


% Req/reply events

check(Name) ->
    utils:log("API Check Request"),
    gen_statem:call({global, Name}, check).


check_reply(Reply) ->
    utils:log("API Check Reply"),
    {_Sender, Target, _SendingTime, _RTT, _Body} = Reply,
    gen_statem:call({global, Target}, {check_reply, Reply}).


crossing(Req) ->
    utils:log("API Crossing Request"),
    {_Label, _Sender, Target, _SendingTime, _Body} = Req,
    gen_statem:call({global, Target}, {crossing, Req}).
        
