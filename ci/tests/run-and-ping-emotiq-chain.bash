#!/usr/bin/env bash

BASE="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && cd ../.. && pwd)"

source ${BASE}/ci/tests/chain-functions.bash

pushd $BASE/demo-network

./copy-sample-configs.bash

if ./start-blockchain.bash ; then
  echo "Emotiq blockchain started ok!"
else
  cleanup
  exit 1
fi

if ./ping-nodes.bash ; then
  echo "Nodes are responding ok!"
else
  cleanup
  exit 1 
fi

./stop-blockchain.bash

popd
