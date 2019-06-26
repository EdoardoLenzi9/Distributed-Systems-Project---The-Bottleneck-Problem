git clone https://github.com/davisp/jiffy.git
cd jiffy
make
cp -r ./priv ./ebin
cp -r ./ebin/* ../car
cd .. 
rm -rf ./jiffy