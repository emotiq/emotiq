#!/usr/bin/env bash
set -x

LISP=${LISP:-ccl}

if test $# -eq 0; then
  echo "Usage: $0 lisp-source-file"
  exit 1
fi

tempfoo=`basename $0`
TMPFILE=`mktemp /tmp/${tempfoo}.XXXXXX` || exit 1

case $LISP in
  ccl*)
    rlwrap ccl -l $1
    ;;
  lispworks*)
    cat $1 >>$TMPFILE
    echo '(loop while t do (sleep 1))' >> $TMPFILE
    rlwrap lwpro <$TMPFILE
    ;;
  *)
    echo "Unknown CommonLisp dialect: $LISP. Falling back to CCL"
    rlwrap ccl -l $1
    ;;
esac

rm -rf $TMPFILE
