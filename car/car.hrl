%%%===================================================================
%%% macros and record definitions
%%%===================================================================

-record (carState, {    name,
arrivalTime, 
delta,
timeout, 
adj }).


-record (adj, { frontCars, 
rearCars }).