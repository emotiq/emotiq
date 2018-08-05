#!/usr/bin/env bash

ccl -l /root/work/emotiq/etc/setup-emotiq-quicklisp.lisp </dev/null

# Link library
/root/work/emotiq/etc/build-crypto-ecc.bash
/root/work/emotiq/etc/build-crypto-pairings.bash
ccl -e "(ql:quickload :emotiq/startup)" -e "(emotiq:main)"
