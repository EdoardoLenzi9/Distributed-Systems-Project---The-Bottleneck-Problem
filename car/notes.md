-module(car).
-record(adj, {  front,
                rear }).

start() ->
    inets:start(),
    alert_youself(),
    get_adj_cars(),
    berkeley().


alert_youself() ->
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = httpc:request(get, {"http://localhost:8086/car?car_name=A", []}, [], []).


get_adj_cars() ->
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = httpc:request(get, {"http://localhost:8086/adj?car_name=A", []}, [], []).


berkeley() ->
    "se arriva un blocco da due macchinine o l'ultima contatta la penultima prima che questa contatta la chiudifila 
    oppure non e' un blocco!",
    "A B Puo farlo al contrario
    A chiama B
    A conosce RTT e tempo di B
    A invia a B delta = t_A - (t_B + RTT/2)
    B setta".


alert_position() ->
    "B deve dire alla macchina con distanza p di esistere
    deve propagarsi lungo la catena fino al leader".


ascolta() ->
    "diventa leader, 
    leader gli dice di passare -> e' in un blocco".


diventa_leader()->
    "il vecchio leader gli passa l'ordine corrente,
    --e' in ascolto di altra gente che arriva--
    --mette in comunicazione i prossimi capofila--
    passa e decide con chi passare,
    leader election,
    passa l'ordine".


passare() ->
    "delay, deve continuare a restare in ascolto,
    guasti
    comunica al web svc
    kill processo".


note() ->
    "le macchine non si muovono finche qualcuno no gli dice di muoversi".