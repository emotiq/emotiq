#!/usr/bin/env bash

if test $# -eq 0; then
  echo "Usage: $0"
  exit 1
fi

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

~/bin/lwpro <$tmpfile

if test $? -eq 0 ; then
  rm $tmpfile
else
  echo "Error in script. Check the source: $tmpfile"
fi
