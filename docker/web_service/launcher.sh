#!/bin/bash
sudo docker build -t mywebservice:v2 .
sudo docker run -it -p 8090:8090 mywebservice:v2   