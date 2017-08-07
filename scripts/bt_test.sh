#!/bin/bash

WHEREAMI="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $WHEREAMI/test_env.sh

$SCRIPTS_PREFIX/bt_cleanup.sh
$SCRIPTS_PREFIX/bt_setup.sh
sleep 0.1
$SCRIPTS_PREFIX/bt_tracker.sh 2> /dev/null &
sleep 0.1

for i in $(seq 1 $NUM_CLIENTS)
do
    DIR=$TEST_PATH_PREFIX$i 
    LOG=$DIR/log
    echo "start rpc_server $i"
    # $SCRIPTS_PREFIX/bt_rpc_server.sh $i &> $LOG & 
    $SCRIPTS_PREFIX/bt_rpc_server.sh $i & 
    sleep 0.05
done
sleep $WAITING
FILE=$TORRENT_DIR/$TORRENT
echo $EXEC_PREFIX/$RPC_CLIENT -r $BASE_RPC_PORT -n $NUM_CLIENTS $FILE
$EXEC_PREFIX/$RPC_CLIENT -r $BASE_RPC_PORT -n $NUM_CLIENTS $FILE
pkill -f tracker
pkill -f rpc_server

