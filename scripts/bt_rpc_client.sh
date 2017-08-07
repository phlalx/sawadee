#!/bin/bash

WHEREAMI="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $WHEREAMI/test_env.sh

echo "starting RPC client on port $RPC_PORT"
$EXEC_PREFIX/$RPC_CLIENT -r $RPC_PORT