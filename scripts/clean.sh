#!/bin/bash

rm *.beam
rm *.dump
rm rebar.lock

# Clean folder car/
cd car
sh clean.sh
cd ..

# Clean folder report/
cd report
sh clean.sh
cd ..

# Clean folder web_service/
cd web_service
sh clean.sh
cd ..