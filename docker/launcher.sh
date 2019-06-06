#!/bin/bash
sudo docker build -t myapp:v4 .                                                 # creates an image with name:tag
#sudo docker run -it --entrypoint "erl" myapp:v4 -sname process1 -run helloworld start   # starts a new container
sudo docker run -d --entrypoint "erl" myapp:v4 -sname process1 -run helloworld start   # starts a new container