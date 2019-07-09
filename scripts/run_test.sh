#!/bin/bash

for filename in *.erl; do
    erlc "$filename"
done

for filename in *test.erl; do
    fn=${filename%.*}
    erl -noshell -eval ""$fn":test()" -eval 'init:stop()'
done