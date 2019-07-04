
start:
	git clone https://github.com/davisp/jiffy.git ; \
	cd jiffy ; \
	make ; \
	cp -r ./priv ./ebin ; \
	cp -r ./ebin/* ../car ; \
	cd .. ; \
	rm -rf ./jiffy

car.beam: car/car.erl
	cd car/ ; \
	erlc car.erl ; \
	cd .. 



#* poter lanciare i test come nodi erl -sname car1@car1 -run normal_state_test test
#
#* spostare file e cartelle
#
#* fare in modo ordinato tutto quello che c'e' in setup.sh (clonare jiffy, compilare, buttare i beam in car/)...
#
#erlc "car.erl"
#erl -name car1@car1 -run car bs 