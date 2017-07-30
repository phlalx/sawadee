#!/bin/sh
WHEREAMI="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $WHEREAMI/test_env.sh

echo "cleaning up"
for i in $(seq 1 $NUM_CLIENTS)
do
    DIR=$TEST_PATH_PREFIX$i 
    echo rm -rf $DIR
    rm -rf $DIR
done
