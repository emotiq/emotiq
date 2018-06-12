#!/usr/bin/env bash

# Link library
/root/work/emtotiq/etc/build-crypto-pairings.bash
ccl -e "(ql:quickload :emotiq/startup)" -e "(emotiq:main)"
