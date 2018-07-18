#!/usr/bin/env bash

BASE="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

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

case ${LISP} in
    lispworks*)
        ls=''
        ;;
    ccl*)
        lisp_cli="$HOME/bin/ccl"
        ;;
    sbcl*)
        lisp_cli="/usr/local/bin/sbcl --disable-debugger"
        ;;
    *)
        echo "Unknown Lisp dialect: $LISP, falling back to CCL"
        lisp_cli="$HOME/bin/ccl"
        ;;
esac

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
tmux new-session -d -s node${node_id} "$BASE/run-in-repl.bash $TMPFILE"

${timeout_cli} ${start_timeout} sh -c 'until nc -z $0 $1 2> /dev/null; do echo "Sleeping 1 sec..."; sleep 1; done' localhost $((3139+${node_id}))

if nc -z localhost $((3139+${node_id})) 2>/dev/null ; then
  echo "Ok!"
else
  echo "Couldn't connect to node for ${start_timeout} seconds. Exiting..."
  rc=1
fi

rm $TMPFILE
exit ${rc}
