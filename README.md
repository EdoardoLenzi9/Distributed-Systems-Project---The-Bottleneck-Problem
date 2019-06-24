{receiver, ab@eddy} ! crash.
erl -sname ab -run car_supervisor start ab

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