#!/bin/bash

WHEREAMI="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $WHEREAMI/test_env.sh

$SCRIPTS_PREFIX/bt_cleanup.sh
$SCRIPTS_PREFIX/bt_setup.sh
$SCRIPTS_PREFIX/bt_tracker.sh 2> /dev/null &
sleep 0.1

for (( i=0; i<$NUM_CLIENTS; i++ ))
do
    DIR=$TEST_PATH_PREFIX$i 
    LOG=$DIR/log
    $SCRIPTS_PREFIX/bt_rpc_server.sh $i & 
    sleep 0.1
done
FILE=$TORRENT_DIR/$TORRENT
sleep 5 # wait for DHT to be initialized
echo $EXEC_PREFIX/$TEST_TORRENT -r $BASE_RPC_PORT -n $NUM_CLIENTS -s $TIME_TO_WAIT $FILE
$EXEC_PREFIX/$TEST_TORRENT -r $BASE_RPC_PORT -n $NUM_CLIENTS -s $TIME_TO_WAIT $FILE

