#!/bin/bash
erlc "car.erl"
erl -name car1@car1 -run car bs 