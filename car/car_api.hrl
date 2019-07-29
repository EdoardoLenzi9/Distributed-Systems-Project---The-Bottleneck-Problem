%% @author Edoardo Lenzi, Talissa Dreossi
%% @copyright GPL-3
%% @version 1.0.0


%%%===================================================================
%%% API for supervisor to call its car
%%%===================================================================


-compile( export_all ).


%% @doc FSM entry point

start_link( Name, State ) ->
    utils:log( "API Start link" ),
    gen_statem:start( { global, Name }, ?MODULE, [ State ], [ ] ).
 

%% @doc stop FSM

stop( Name ) ->
    utils:log( "API Stop" ),
    gen_statem:stop( { global, Name } ).


%%%===================================================================
%%% Simple events
%%%===================================================================


%% @doc tow_truck event
    
tow_truck( Name ) ->
    utils:log( "API tow_truck" ),
    gen_statem:call( { global, Name }, tow_truck ).


%% @doc default_behaviour event

default_behaviour( Name ) ->
    utils:log( "API default_behaviour" ),
    gen_statem:call( { global, Name }, default_behaviour ).
  
    
%% @doc crash event

crash( Name, CrashType ) ->
    utils:log( "API Crash" ),
    gen_statem:call( { global, Name }, { crash, CrashType } ).


%% @doc adj_reply event

adj_reply( Name, Adj ) ->
    utils:log( "API Adj Reply" ),
    gen_statem:call( { global, Name }, { adj_reply, Adj } ).
    

%% @doc last_adj_reply event

last_adj_reply( Name, Last ) ->
    utils:log( "API Last Adj Reply" ),
    gen_statem:call( { global, Name }, { last_adj_reply, Last } ).


%% @doc update_front event

update_front( Name, Replacement ) ->
    utils:log( "API Update front" ),
    gen_statem:call( { global, Name }, { update_front, Replacement } ).


%% @doc update_rear event

update_rear( Name, Replacement ) ->
    utils:log( "API Update rear" ),
    gen_statem:call( { global, Name }, { update_rear, Replacement } ).


%% @doc timeout event

timeout( Name, Target ) ->
    utils:log( "API Timeout Reply" ),
    gen_statem:call( { global, Name }, { timeout, Target } ).


%%%===================================================================
%%% Req/reply events
%%%===================================================================


%% @doc check event

check( Name, Sender ) ->
    utils:log( "API Check Request" ),
    gen_statem:call( { global, Name }, { check, Sender } ).


%% @doc check_reply event

check_reply( Reply ) ->
    utils:log( "API Check Reply" ),
    { _Sender, Target, _SendingTime, _RTT, _Body } = Reply,
    gen_statem:call( { global, Target }, { check_reply, Reply } ).


%% @doc crossing event

crossing( Req ) ->
    utils:log( "API Crossing Request" ),
    { _Label, _Sender, Target, _Nickname, _SendingTime, Body } = Req,
    gen_statem:call( { global, Target }, { crossing, Body } ).