%%%===================================================================
%%% Behaviour standard callbacks
%%%===================================================================


terminate(_Reason, _StateName, _State) ->
    ok.
 

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


callback_mode()-> state_functions.