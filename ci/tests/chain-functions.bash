#!/usr/bin/env bash

function cleanup() {
  for (( i=1 ; i<=${1} ; i++ )) ; do
    echo Stopping Node ${i}
    tmux kill-session -t node${i}
  done
}
