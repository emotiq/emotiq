#!/usr/bin/env bash

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EMOTIQ_ROOT=$DIR/..
nodes_count=${1:-3}

${DIR}/update_hosts_conf.py ${nodes_count}

# Pull in all dependencies
$EMOTIQ_ROOT/ci/lisp-wrapper.bash -e '(ql:quickload :emotiq/startup)'
status=$?

if  [ $status -ne 0 ] ; then
  echo "Failed to quickload Emotiq dependencies"
  exit 1
fi

# Stop running node, if any
${DIR}/stop-blockchain.bash > /dev/null 2>&1

# Start the Emotiq blockchain
for (( i=2 ; i<=${nodes_count} ; i++ )) ; do
  if $DIR/start-node.bash $i ; then
    echo Node ${i} started...
  else
    echo Failed to start Node ${i}. Exiting
    exit 1
  fi
done

echo ${nodes_count}-node Emotiq network started!
