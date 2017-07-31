#!/bin/bash

WHEREAMI="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $WHEREAMI/test_env.sh

# server listens on 6969
echo "starting server"
$EXEC_PREFIX/tracker_server.bc
