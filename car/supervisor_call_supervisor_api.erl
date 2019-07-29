%% @author Edoardo Lenzi, Talissa Dreossi
%% @copyright GPL-3
%% @version 1.0.0


%%%===================================================================
%%% API for a supervisor to call/respond another supervisor
%%%===================================================================


-module( supervisor_call_supervisor_api ).
-compile( export_all ).
-include( "car.hrl" ).


%% @doc function used when a timer needs to call the supervisor of another car

timer_call( Req ) ->
    utils:log( "Timer call supervisor" ),
    { _Label, _Sender, Target, _Nickname, _SendingTime, _Body } = Req,
    call_supervisor( Target, { timer_call, Req } ). 


%% @doc call primitive

call_supervisor( Name, Event ) ->
    utils:log( "send event" ),
    { supervisor, Name } ! Event.