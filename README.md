# The Bottleneck Problem - Distributed Systems Project 2019

The aim of this project is the analysis, the implementation and testing of a 
distributed solution for the **bottleneck problem** 
(also know as \textit{The Monkeys Problem}, in operative systems theory).

Basically the problem consists in a two-way road with a **bridge** (bottleneck) 
in the middle, the bridge has a certain **maximum capacity** and can be crossed 
in only **one direction at a time**.

The bridge is **very risky** cause it is located in a remote place where 
there is nothing that can prevent **car crashes/congested traffic** but cars. \\

Note that **cars** here are **autonomous systems** without any human driver
inside and can send messages with others adjacent cars with some wireless technologies 
(ie. bluetooth, wifi, ...) in order to solve the situation.\\    

The projet requires the implementation of a **simulator/business logic** for the 
environment setup and a **web server** that will expose the simulation state 
with some API for any further **UI application**.


## Team 

* Edoardo Lenzi
* Talissa Dreossi


## Sprints and Deadlines

The project deadline is 28 of June; we ar going to split the month into 4 sprints of a week:
* Sprint 1 (28 May) **interconnections**
    * Docker container with Docker container
    * Erlang and Bash
    * HTML client with Erlang Web Service (simple mok)
    * Erlang with Mnesia or SQL/SQL Lite/ERTS
    * Create a Docker Image with an embedded Erlang script
    * Spawn a new Docker Container with a bash script
> At the end of this sprint we will have an architecture like this one
> <br/>
>![Sprint0](report/assets/sprint0.png)
* Sprint 2 (04 June - 09 June) **connections**
* Sprint 3 (09 June - 16 June) **business logic**
* Sprint 3 (16 June - 18 June) **testing**
* Sprint 4 (18 June - 25 June) **UI and report**
* 3 days for code refactoring

## Get Started

```
sh clean.sh
sh setup.sh
cd web_service/client/
npm install
```

## TODO

* Creare un makefile
* Tutti i test anche della macchina


# Note

Superare le macchine non e' FIFO


# Eventi

{check, Req} 
{check_reply, Response}
{crossing, Req}
{crossing_reply, Response}
{update, Req} 

{adj_reply, Response}

crash
default_behaviour

next




Sync
* Ha gia la lista dei sync, lancia un check al piu' vicino
    * se non gli risponde entro max_RTT e' morto e chiamo tow_truck
    * se e' morto l'update della lista prevede il replace del morto, uno shift della lista
        se la potenza e' maggiore di 1 e un check all'ultimo per ritornare ad una situazione consistente
    * Quando mi arriva un update lancio un nuovo check (vado in default behaviour)
* Alla fine devo essere sincronizzato o davanti non ho nessuno
* Chiamo Adj per notiziare che esisto

* Se io tardassi a sincronizzarmi e, dall'altra parte le macchine pur arrivando dopo si sincronizzassero prima comunque appena mi sincronizzo il mio arrival time fa fede e, nel momento del crossing non dovrei aver problemi


Dead
* una volta morta la macchina deve mettere in comunicazione, manda a quello dietro il primo davanti a lei in un messaggio di update 