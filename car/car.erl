%% @author Edoardo Lenzi, Talissa Dreossi
%% @copyright GPL-3
%% @version 1.0.0


-module( car ).
-behaviour( gen_statem ).

-include( "car_std.hrl" ).
-include( "car_api.hrl" ).


%%%===================================================================
%%% gen_statem states
%%%===================================================================


%% @doc Init metastate useful to initialize State parameters 

init( [ State ] ) ->
    utils:log( "---STATE Init---" ),
    init_handler:init( State ).
    

%% @doc Sync state used for car synchronization ( Berkeley ) and adj queue update
%%      before next() car doesn't exists for the simulation and after that car 
%%      must be synchronized despite concurrency issues 

sync( { call, From }, Event, Data ) ->
    utils:log( "---STATE Sync---" ),
    sync_handler:sync( From, Event, Data ).


%% @doc Normal state manages car movements, if a car has not any impediment in front 
%%      can runs at max_speed towards the end of the bridge; once the car reaches the bridge
%%      go the the leader state (if previously a leader does not send a crossing event, in that 
%%      case car remains in normal until the end of the bridge).

normal( { call, From }, Event, Data ) ->
    utils:log( "---STATE Normal---" ),
    normal_handler:normal( From, Event, Data ).


%% @doc Leader state manages crossing priority; firstly sends a check to the car on the opposite side (if present)
%%      then propagate a crossing event to the rear cars if the arrival time of the front car is higher that its arrival time

leader( { call, From }, Event, Data ) ->
    utils:log( "---STATE Leader---" ),
    leader_handler:leader( From, Event, Data ).


%% @doc Car reaches dead state if it reaches the end of the bridge or crashes; anyway soon the car will be killed so 
%%      it has the duty to give this information to the nearest cars

dead( { call, From }, Event, Data ) -> 
    utils:log( "---STATE Dead---" ),
    dead_handler:dead( From, Event, Data ).