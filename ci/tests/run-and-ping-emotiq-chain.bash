#!/usr/bin/env bash

BASE="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && cd ../.. && pwd)"

pushd $BASE/demo-network

./start-blockchain.bash || (echo Failed to start blockchain ; exit 1)
./ping-nodes.bash || (echo Failed to ping nodes ; exit 1)
./stop-blockchain.bash

popd
