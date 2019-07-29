%% @author Edoardo Lenzi, Talissa Dreossi
%% @copyright GPL-3
%% @version 1.0.0


%%%-------------------------------------------------------------------
%% @doc web_service top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module( web_service_sup ).
-behaviour( supervisor ).
-export( [ start_link/0 ] ).
-export( [ init/1 ] ).

-define( SERVER, ?MODULE ).
-spec start_link() -> { ok, pid() }.


start_link() ->
    supervisor:start_link( { local, ?SERVER }, ?MODULE, [ ] ).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional

init( [ ] ) ->
    SupFlags = #{ strategy => one_for_all,
                  intensity => 10,
                  period => 10
                },
    ChildSpecs = [ ],
    db_manager:start(),
    { ok, { SupFlags, ChildSpecs } }.

