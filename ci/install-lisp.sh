#!/usr/bin/env bash

case ${LISP} in
    lispworks*)
        ci/install-lispworks-$TRAVIS_OS_NAME.sh
        ;;
    ccl*)
        ci/install-ccl.sh
        ;;
esac
