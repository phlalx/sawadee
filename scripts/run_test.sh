#!/bin/bash

WHEREAMI="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $WHEREAMI/test_env.sh

$EXEC_PREFIX/cleanup_test.sh
$EXEC_PREFIX/setup_test.sh
sleep 0.1
$EXEC_PREFIX/start_server.sh 2> /dev/null &
sleep 0.1

for i in $(seq 1 $NUM_CLIENTS)
do
    DIR=$TEST_PATH_PREFIX$i 
    LOG=$DIR/log
    echo "start client $i"
    $EXEC_PREFIX/start_client.sh $i &> $LOG & 
    sleep 0.05
done
sleep 30.0 
pkill -f main.byte 
pkill -f tracker_server.byte
RES=`grep "written 100%" download*/log | wc -l`
if [ $RES = $NUM_CLIENTS ]
then 
  echo "OK"
  exit 0
else
  echo "KO $RES != $NUM_CLIENTS "
  exit 1
fi


