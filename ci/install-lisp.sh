#!/usr/bin/env bash

case ${LISP} in
    lispworks*)
        ci/install-lispworks.sh
        ;;
    ccl*)
        ci/install-ccl.sh
        ;;
esac
