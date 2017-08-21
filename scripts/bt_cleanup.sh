#!/bin/bash
WHEREAMI="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $WHEREAMI/test_env.sh

echo "cleaning up"
for (( i=0; i<$NUM_CLIENTS; i++ ))
do
    DIR=$TEST_PATH_PREFIX$i 
    echo rm -rf $DIR
    rm -rf $DIR
done
echo pkill -f $CLIENT
echo pkill -f $RPC_CLIENT
echo pkill -f $RPC_SERVER
echo pkill -f $TRACKER

pkill -f $CLIENT
pkill -f $RPC_CLIENT
pkill -f $RPC_SERVER
pkill -f $TRACKER