#/bin/bash

# setup car 


## setup jiffy

git clone https://github.com/davisp/jiffy.git
cd jiffy
make
cp -r ./priv ./ebin
cp -r ./ebin/* ../car
cd .. 
rm -rf ./jiffy


## compile car

cd car/ 
sh ../compile-all.sh 
cd ..


# Setup report 

cd report/
pdflatex *.tex
cd ..
# Setup web_service 

cd web_service/client
# TODO setup different location if u have setted another npm path on your pc
npm install 
cd ..
rebar3 compile
cd ..


# Setup environment

cp environment.json car/
cp environment.json web_service/
cp environment.json web_service/client/
cp environment.json web_service/client/views/simulation-view/


# Setup test

cd web_service/test 
cp -r ../apps/web_service/src .
mv src tmp
cp *.erl ./tmp/
cp ../apps/web_service/src/*.erl ./tmp/
cp ../apps/web_service/src/*.hrl ./tmp/
cp ../../car/jiffy.app ./tmp/
cp ../../car/jiffy.beam ./tmp/
cp -r ../../car/priv ./tmp/

cd tmp 

for filename in *.erl; do
    erlc "$filename"
done

for filename in *test.erl; do
    fn=${filename%.*}
    erl -noshell -eval ""$fn":test()" -eval 'init:stop()'
done
#cd ..
#rm -rf ./tmp