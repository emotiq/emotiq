#!/usr/bin/env bash

BASE="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && cd ../.. && pwd)"
nodes_count=${1:-3}

source ${BASE}/ci/tests/chain-functions.bash

pushd $BASE/demo-network

./copy-sample-configs.bash

if ./start-blockchain.bash ${nodes_count}; then
  echo "Emotiq blockchain started ok!"
else
  cleanup ${nodes_count}
  exit 1
fi

if ./ping-nodes.bash ${nodes_count}; then
  echo "Nodes are responding ok!"
else
  cleanup ${nodes_count}
  exit 1
fi

./stop-blockchain.bash ${nodes_count}

popd
