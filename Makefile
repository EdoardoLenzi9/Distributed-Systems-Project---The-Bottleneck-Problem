.PHONY: start run test clean help 

Targets := $(wildcard *.erl)
TargetsTest := $(wildcard *test.erl)


# public rules

start: clean dependencies env build


run:
	@echo "make rule run"
	make dependencies


test:
	@echo "make rule test"
	@cd web_service/test 
	@pwd
	@cp -r ../apps/web_service/src .
	@mv src tmp 
	@cp *.erl ./tmp/ 
	@cp ../apps/web_service/src/*.erl ./tmp/ 
	@cp ../apps/web_service/src/*.hrl ./tmp/ 
	@cp ../../car/jiffy.app ./tmp/ 
	@cp ../../car/jiffy.beam ./tmp/ 
	@cp -r ../../car/priv ./tmp/ 


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
	@echo Requirements: git, npm, erlang, docker-ce 


clean:
	@echo "delete compiled/error files"
	@find . -type f -name '*.beam' -delete
	@find . -type f -name '*.lock' -delete
	@find . -type f -name '*.dump' -delete
	@find . -type f -name '*.aux' -delete
	@find . -type f -name '*.log' -delete
	@find . -type f -name '*.out' -delete
	
	@echo "delete jiffy"
	@rm -rf ./jiffy
	@rm -rf ./car/priv
	@rm ./car/jiffy.app || true
	
	@echo "clean env"
	@rm ./car/environment.json  || true
	@rm ./web_service/environment.json  || true
	@rm ./web_service/client/environment.json  || true
	@rm ./web_service/client/views/simulation-view/environment.json  || true

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
	cd ..


env:
	@echo "make rule env"
	@cp environment.json car/ 
	@cp environment.json web_service/ 
	@cp environment.json web_service/client/ 
	@cp environment.json web_service/client/views/simulation-view/ 



build: 
	@echo "make rule build"
	@cd car/ ; \
	sh ../scripts/compile-all.sh