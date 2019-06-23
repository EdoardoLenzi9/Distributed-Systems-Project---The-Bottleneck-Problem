-record (state, {   name,
                    arrivalTime, 
                    delta,
                    timeout, 
                    adj }).


-record (adj, { frontCars, 
                rearCars }).