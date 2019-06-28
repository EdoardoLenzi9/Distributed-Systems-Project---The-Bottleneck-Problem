#!/bin/bash

cp -r ../apps/web_service/src .
mv src tmp
cp *.erl ./tmp/
cp ../apps/web_service/src/*.erl ./tmp/
cp ../apps/web_service/src/*.hrl ./tmp/
#TODO
#sudo sh ../../setup_jiffy.sh
cd ./tmp 

for filename in *.erl; do
    erlc "$filename"
done

for filename in *test.erl; do
    fn=${filename%.*}
    erl -noshell -eval ""$fn":test()" -eval 'init:stop()'
done
#cd ..
#rm -rf ./tmp