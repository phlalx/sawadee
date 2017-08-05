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
    echo "start client $i"
    $SCRIPTS_PREFIX/bt_client.sh $i &> $LOG & 
    sleep 0.05
done
sleep $WAITING
echo pkill -f $EXEC_PREFIX/$CLIENT 
echo pkill -f $EXEC_PREFIX/$TRACKER
pkill -f $EXEC_PREFIX/$CLIENT 
pkill -f $EXEC_PREFIX/$TRACKER
grep "written 100%" download*/log
RES=`grep "written 100%" download*/log | wc -l`
if [ $RES = $NUM_CLIENTS ]
then 
  echo "OK"
  exit 0
else
  echo "KO $RES != $NUM_CLIENTS "
  exit 1
fi


