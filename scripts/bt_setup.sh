#!/bin/bash
WHEREAMI="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $WHEREAMI/test_env.sh

echo "setting up tests"

for i in $(seq 1 $NUM_CLIENTS)
do
    DIR=$TEST_PATH_PREFIX$i 
    echo "setting up dir $DIR"
    cp -r $FILES $DIR
    echo "$SCRIPTS_PREFIX/gen_bitset.py "$DIR/$BITSET" $BITSET_LEN"
    $SCRIPTS_PREFIX/gen_bitset.py "$DIR/$BITSET" $NUM_PIECES
done
 


