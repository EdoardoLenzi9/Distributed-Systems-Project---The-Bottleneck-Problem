Targets := $(wildcard *.erl)
TargetsTest := $(wildcard *test.erl)

start:
	git clone https://github.com/davisp/jiffy.git ; \
	cd jiffy ; \
	make ; \
	cp -r ./priv ./ebin ; \
	cp -r ./ebin/* ../car ; \
	cd .. ; \
	rm -rf ./jiffy

%.beam: car/$(Targets)
	erlc car/*.erl ; \
	cd web_service/client ;\
	npm install ;\
	cd .. ;\
	rebar3 compile ;\
	cd .. ;\
	make env

env:
	cp environment.json car/ ;\
	cp environment.json web_service/ ;\
	cp environment.json web_service/client/ ;\
	cp environment.json web_service/client/views/simulation-view/ ;\
	make test

test:
	cd web_service/test  ;\
	cp -r ../apps/web_service/src . ;\
	mv src tmp ;\
	cp *.erl ./tmp/ ;\
	cp ../apps/web_service/src/*.erl ./tmp/ ;\
	cp ../apps/web_service/src/*.hrl ./tmp/ ;\
	cp ../../car/jiffy.app ./tmp/ ;\
	cp ../../car/jiffy.beam ./tmp/ ;\
	cp -r ../../car/priv ./tmp/ 

#	erl -sname car1@car1 -run normal_state_test test

# TO DO 
# fare parte test

	#* poter lanciare i test come nodi erl -sname car1@car1 -run normal_state_test test

#erlc "car.erl"
#erl -name car1@car1 -run car bs 