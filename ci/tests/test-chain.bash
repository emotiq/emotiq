#!/usr/bin/env bash

BASE="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEVNET_CONFIGS_PATH=$BASE/../node-configs
EMOTIQ_ETC_ROOT=$BASE/../../var/etc

# Copy testing config files
mkdir -p $EMOTIQ_ETC_ROOT
cp -r $DEVNET_CONFIGS_PATH/* $EMOTIQ_ETC_ROOT

# Start the Emotiq blockchain
for i in {1..3} ; do
  if $BASE/start-node.bash $i ; then
    echo Node ${i} started...
  else
    echo Failed to start Node ${i}. Exiting
    exit 1
  fi
done

# Send pings to the nodes
for i in {1..3} ; do
  if python3 $BASE/ws-rpcping.py $i ; then
    echo Node $i pong received
  else
    echo Failed to ping Node ${i}. Skipping following nodes.
    break
  fi
done

# Stop all the nodes
for i in {1..3} ; do
  echo Stopping Node ${i}
  tmux kill-session -t node${i}
done
