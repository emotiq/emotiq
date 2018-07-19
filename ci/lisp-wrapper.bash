#!/usr/bin/env bash

set -x 
echo lisp-wrapper called with:
echo $*

if test $# -eq 0; then
  echo "Usage: $0 -l|--load file or -e|--eval s-exp"
  exit 1
fi

BASE="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source $BASE/functions.bash

IFS=
lisp_exec $@
status=$?

if test $status -eq 0 ; then
  rm $tmpfile
  exit 0
else
  echo "Exit code: ${status}"
  echo "Error in script. Check the source: $tmpfile"
  exit $status
fi
