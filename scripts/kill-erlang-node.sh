#!/bin/bash
# Source: https://gist.githubusercontent.com/robertoaloi/8884096/raw/b2fa3627e673127e2ace75a994d30a1d11f8afb8/kill-erlang-node.sh

# Kill an Erlang process by node name
#
# e.g.: kill-erlang-node kred

# Check usage
if [ -z "$1" ]; then
    echo "Usage: `basename $0` NODE_NAME"
    exit 1
fi

# Fetch input parameters
NAME="$1"

# Kill the Erlang process corresponding to a given node name
port=`epmd -names | awk -v name=$NAME '$2==name {print $5}'`
if [ -z "$port" ]; then
    echo "ERROR: Node name not found: $NAME"
    exit 1
else
    pid=`lsof -i TCP:$port -s TCP:LISTEN | tail -n +2 | awk '{print $2}'`
    kill $pid
    exit 0
fi