.PHONY: start run test clean help 


# public rules

start: clean dependencies build env cred setup-docker


run:
	@echo "make rule run"
	@cat environment.json
	@cd web_service ; \
	sudo ./rebar3 run


test:
	@echo "make rule test"
	@echo "clean tests"
	@rm ./car/test/tmp/* || true
	@rm -rf ./web_service/test/tmp/src || true
	@rm ./web_service/test/tmp/* || true
	@cd web_service/test ; \
	cp -r ../apps/web_service/src . ; \
	mv src tmp ; \
	cp *.erl ./tmp/src ; \
	cd tmp/src ; \
	sh ../../../../scripts/run_test.sh
	@echo "car test"
	@echo "TODO"


help:
	@echo Team:
	@echo " Edoardo Lenzi"
	@echo " Talissa Dreossi"
	@echo Project details:
	@echo " The project provides a distributed solution to the bottleneck problem, see https://github.com/EdoardoLenzi9/Distributed-Systems-Project---The-Bottleneck-Problem for more details"
	@echo Provided Rules:
	@echo " help \t\t prints Make rules."
	@echo " start \t\t sets up dependencies and compile."
	@echo " run \t\t starts the simulation."
	@echo " test \t\t starts test."
	@echo Requirements: git, npm, erlang, rebar3, docker-ce 


clean:
	@echo "delete compiled/error files"
	@find . -type f -name '*.beam' -delete
	@find . -type f -name '*.lock' -delete
	@find . -type f -name '*.dump' -delete
	@find . -type f -name '*.aux' -delete
	@find . -type f -name '*.log' -delete
	@find . -type f -name '*.out' -delete
	@rm -rf web_service/_build
	#TODO
	#@rm -rf web_service/client/node_modules

	@echo "delete jiffy"
	@rm -rf ./jiffy
	@rm -rf ./car/priv
	@rm ./car/jiffy.app || true
	
	@echo "clean logs"
	@cd car/ ; \
	rm -rf logs ; \
	mkdir logs

	@echo "clean env"
	@rm ./car/environment.json  || true
	@rm ./web_service/environment.json  || true
	@rm ./web_service/client/environment.json  || true
	@rm ./web_service/client/views/simulation-view/environment.json  || true
	@rm ./web_service/_build/default/rel/web_service/environment.json  || true
	@rm ./web_service/client/views/log-view/environment.json  || true
	@echo "clean credential"
	@rm ./web_service/_build/default/rel/web_service/credentials.json  || true
	@rm ./car/credentials.json  || true
	@echo "clean tests"
	@rm ./car/test/tmp/* || true
	@rm ./web_service/test/tmp/* || true


# private rules 

dependencies:
	@echo "make rule dependencies"
	@echo "setup jiffy"
	@git clone https://github.com/davisp/jiffy.git ; \
	cd jiffy ; \
	make ; \
	cp -r ./priv ./ebin ; \
	cp -r ./ebin/* ../car ; \
	cp -r ./ebin/* ../web_service/apps/web_service/src
	@echo "setup node modules"
	@cd web_service/client ; \
	npm install || true
	@cd web_service ; \
	wget https://s3.amazonaws.com/rebar3/rebar3 ; \
	chmod +x rebar3


env:
	@echo "make rule env"
	@cp environment.json car/ 
	@cp environment.json web_service/ 
	@cp environment.json web_service/client/ 
	@cp environment.json web_service/client/views/simulation-view/ 
	@cp environment.json web_service/client/views/log-view/ 
	@cp environment.json web_service/_build/default/rel/web_service/


cred:
	@echo "make rule credential"
	@cp credentials/credentials.json car/ || true
	@cp credentials/credentials.json web_service/_build/default/rel/web_service/ || true


setup-docker: build-docker-erlssh build-docker-car build-docker-ws
	@echo "docker setup"


build-docker-erlssh: 
	@echo "create docker network"
	@sudo docker network create ds_network || true
	@echo "build docker erlssh"
	@sudo docker build -t erlssh:v1 .


build-docker-car:
	@echo "build docker car"
	@cd car ; \
	sudo docker build -t car:v1 .


build-docker-ws:
	@echo "build docker ws"
	@cd web_service ; \
	sudo docker build -t webservice:v1 .


build: 
	@echo "make rule build"

	@echo "clean logs"
	@cd car/ ; \
	rm -rf logs ; \
	mkdir logs

	@echo "build car"
	@cd car/ ; \
	sh ../scripts/compile-all.sh
	
	@echo "build web service"
	@cd web_service ; \
	sudo ./rebar3 run || true