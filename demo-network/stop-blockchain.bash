#!/usr/bin/env bash

# Stop all the nodes
for i in {1..3} ; do
  echo Stopping Node ${i}
  tmux kill-session -t node${i}
done
