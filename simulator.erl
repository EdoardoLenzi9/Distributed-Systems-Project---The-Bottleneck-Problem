-module(simulator).

-export[sim/0].

sim()->
    {ok, Pid} = carsfsm:start_link(),
    Pid!crash.