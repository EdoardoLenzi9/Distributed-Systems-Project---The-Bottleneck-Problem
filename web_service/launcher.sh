#!/bin/bash
#sudo docker build -t mywebservice:v1 .
##sudo docker run -p 8090:8090 -it mywebservice:v1                  # Interactive mode
#sudo docker run -p 8090:8090 -dt mywebservice:v1 >> dockerid.txt     # Detached mode

case "$1" in
    build)
        sh cleaner.sh
        (client && npm install)
        echo "start image build"
        sudo docker build -t mywebservice:v1 .
        ;;
    it)
        echo "run container (in iterative mode)"
        sudo docker run -p 8090:8090 -it mywebservice:v1
        ;;
    run)
        echo "run container (in detached mode)"
        sudo docker run -p 8090:8090 -dt mywebservice:v1 >> docker_ps.txt
        ;;
    stop)
        echo "stop containers"
        input="docker_ps.txt"
        while IFS= read -r line
        do
            sudo docker stop "$line"
        done < "$input"
        rm docker_ps.txt;;
    *)
    echo "allowed commands: build, run, stop"  
esac