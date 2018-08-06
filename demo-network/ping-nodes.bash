#!/usr/bin/env bash

nodes_count=${1:-3}

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
rc=0
# Send pings to the nodes
for (( i=1 ; i<=${nodes_count} ; i++ )) ; do
  if python3 $DIR/ws-rpcping.py $i ; then
    echo Node $i pong received
  else
    echo Failed to ping Node ${i}. Skipping following nodes.
    rc=1
    break
  fi
done

if [ $rc -eq 0 ] ; then
  echo "Nodes pinging ok!"
else
  echo "Failure!"
  exit $rc
fi
