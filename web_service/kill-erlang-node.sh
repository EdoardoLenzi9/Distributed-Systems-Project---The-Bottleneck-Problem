#!/bin/bash
COMPLETENAME=$1
NAME=$(echo $COMPLETENAME | cut -d "@" -f 1)
for i in `ps -ef | grep $NAME | awk '{print $2}'`; do echo $i; kill -9 $i; done
for i in `sudo docker ps | grep $NAME | awk '{print $1}'`; do echo $i; sudo docker stop $i; done