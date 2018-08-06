#!/usr/bin/env bash
nodes_count=${1:-3}
# Stop all the nodes
for (( i=1 ; i<=${nodes_count} ; i++ )) ; do
  echo Stopping Node ${i}
  tmux kill-session -t node${i}
done
