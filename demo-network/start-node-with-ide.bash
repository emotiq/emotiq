#!/usr/bin/env bash
#
# Run Emotiq node inside CCL IDE.
#
# Requires Clozure CL64.app to be present in the system.
# If its named differently, change value in the open command
#

LISP=${LISP:-ccl}

if [ ! $LISP = "ccl" ] ; then
  echo Only CCL is supported
  exit 1
fi

if [[ ! $(uname -s) = Darwin* ]] ; then
  echo Only macOS is supported
  exit 1
fi

BASE="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

start_timeout=30
node_id=${1:-1}
rc=0

tempfoo=`basename $0`
TMPFILE=`mktemp /tmp/${tempfoo}.XXXXXX` || exit 1

cat >$TMPFILE <<EOF
(ql:quickload :emotiq/startup)
(emotiq:main :config-subpath "node${node_id}/")
EOF

echo Starting node${node_id} ...
open -a "Clozure CL64" $TMPFILE

gtimeout ${start_timeout} sh -c 'until nc -z $0 $1 2> /dev/null; do echo "Sleeping 1 sec..."; sleep 1; done' localhost $((3139+${node_id}))

if nc -z localhost $((3139+${node_id})) 2>/dev/null ; then
  echo "Ok!"
else
  echo "Couldn't connect to node for ${start_timeout} seconds. Exiting..."
  rc=1
fi
