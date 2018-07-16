#!/usr/bin/env bash

# Start the Emotiq blockchain

for i in {1..3} ; do
  if ./start-node.bash $i ; then
    echo Node ${i} started...
  else
    echo Failed to start Node ${i}. Exiting
    exit 1
  fi
done

# Send pings to the nodes

for i in {1..3} ; do
  if python3 ws-ping.py $i ; then
    echo Node $i pong received
  else
    echo Failed to ping Node ${i}. Exiting
    exit 1
  fi
done

# Stop all the nodes

for i in {1..3} ; do
  echo Stopping Node ${i}
  tmux kill-session -t node${i}
done
