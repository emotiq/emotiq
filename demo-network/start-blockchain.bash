#!/usr/bin/env bash

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EMOTIQ_ROOT=$DIR/..

# Pull in all dependencies
$EMOTIQ_ROOT/ci/lisp-wrapper.bash -e '(ql:quickload :emotiq/startup)'

# Start the Emotiq blockchain
for i in {1..3} ; do
  if $DIR/start-node.bash $i ; then
    echo Node ${i} started...
  else
    echo Failed to start Node ${i}. Exiting
    exit 1
  fi
done

echo 3-node Emotiq network started!
