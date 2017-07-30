#!/bin/sh

WHEREAMI="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $WHEREAMI/test_env.sh

i=1
if test $1 
then
    i=$1
fi
DIR=$TEST_PATH_PREFIX$i 
PORT=$((BASE_PORT+i))
./main.byte -l $PORT -p $DIR $TORRENT_DIR/$TORRENT $VERBOSE
