#!/usr/bin/env bash

# set -x

case $(uname -s) in
    Linux*)
        timeout_cli=timeout
        ;;
    Darwin*)
        timeout_cli=gtimeout
        ;;
    CYGWIN_NT*)
        timeout_cli=timeout
        ;;
    *)
        echo Unknown OS \"$(uname_s)\". Terminating...
        exit 127
        ;;
esac

start_timeout=30
node_id=${1:-1}

tempfoo=`basename $0`
TMPFILE=`mktemp /tmp/${tempfoo}.XXXXXX` || exit 1

cat >$TMPFILE <<EOF
(ql:quickload :emotiq/startup)
(emotiq:main :config-subpath "node${node_id}/")
EOF

echo Starting node${node_id} ...
tmux new-session -d -s node${node_id} "rlwrap ccl -l $TMPFILE"

${timeout_cli} ${start_timeout} sh -c 'until nc -z $0 $1 2> /dev/null; do echo "Sleeping 1 sec..."; sleep 1; done' localhost $((3139+${node_id}))

if nc -z localhost $((3139+${node_id})) 2>/dev/null ; then
  echo "Ok!"
else
  echo "Couldn't connect to node for ${start_timeout} seconds. Exiting..."
  exit 1
fi
