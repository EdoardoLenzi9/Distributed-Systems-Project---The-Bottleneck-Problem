rm rebar.lock 
rm -rf _build
rm environment.json
rm -rf test/tmp
cd apps/web_service/src/
rm *.beam
rm *.dump
cd ../../..

cd client/
rm -rf node_modules/
rm -rf logs/
rm package-lock.json
rm environment.json 
cd ..