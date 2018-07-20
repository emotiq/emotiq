#!/usr/bin/env bash

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
rc=0
# Send pings to the nodes
for i in {1..3} ; do
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
