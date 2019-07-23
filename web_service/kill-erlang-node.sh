#!/bin/bash
COMPLETENAME=$1
NAME=$(echo $COMPLETENAME | cut -d "@" -f 1)
for i in `ps -ef | grep $NAME | awk '{print $2}'`; do echo $i; kill -9 $i; done