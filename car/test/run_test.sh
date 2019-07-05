#!/bin/bash

cp ../*.erl ./tmp/
cp ../*.hrl ./tmp/

#sudo sh ../../setup_jiffy.sh
cd ./tmp 

for filename in *.erl; do
    erlc "$filename"
done

for filename in *test.erl; do
    fn=${filename%.*}
    erl -noshell -eval ""$fn":test()" -eval 'init:stop()'
done
cd ..
rm -rf ./tmp