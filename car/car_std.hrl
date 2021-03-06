%% @author Edoardo Lenzi, Talissa Dreossi
%% @copyright GPL-3
%% @version 1.0.0


%%%===================================================================
%%% Behaviour standard callbacks
%%%===================================================================


terminate( Reason, StateName, State ) ->
    utils:log( "Car terminate ~n Reason: ~p ~n StateName: ~p ~n State: ~p", [ Reason, StateName, State ] ),
    ok.
 

code_change( _OldVsn, StateName, State, _Extra ) ->
    { ok, StateName, State }.


callback_mode()-> state_functions.