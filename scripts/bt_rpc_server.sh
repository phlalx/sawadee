#!/bin/bash

WHEREAMI="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $WHEREAMI/test_env.sh

i=1
if test $1 
then
    i=$1
fi
DIR=$TEST_PATH_PREFIX$i 
PORT=$((BASE_PORT+i))
RPC_PORT=$((BASE_RPC_PORT+i))
echo "starting rpc server on port -l $PORT -r $RPC_PORT" 
$EXEC_PREFIX/$RPC_SERVER -l $PORT -p $DIR $VERBOSE -r $RPC_PORT
