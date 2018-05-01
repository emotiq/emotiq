#!/usr/bin/env bash

if test $# -eq 0; then
  echo "Usage: $0"
  exit 1
fi

case ${LISP} in
    lispworks*)
        lisp_cli='~/bin/lwpro'
        ;;
    ccl*)
        lisp_cli='~/bin/ccl'
        ;;
    sbcl*)
        lisp_cli='/usr/local/bin/sbcl'
        ;;
    *)
        echo "Unknown Lisp dialect: $LISP"
        exit 126
        ;;
esac


tmpfile=$(mktemp /tmp/lwpro-script.XXXXXX)

while test $# -gt 0; do
  case "$1" in
    --eval|-e)
      echo "$2" >> $tmpfile
      shift
      shift
      ;;
    --load|-l)
      echo "(load \"$2\")" >> $tmpfile
      shift
      shift
      ;;
    *)
      echo "Unknown argument $1!"
      exit 1
      ;;
  esac
done

$lisp_cli $@ <$tmpfile

if test $? -eq 0 ; then
  rm $tmpfile
  exit 0
else
  EXIT_CODE=$?
  echo "Error in script. Check the source: $tmpfile"
  exit $EXIT_CODE
fi
