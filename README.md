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