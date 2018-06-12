#!/usr/bin/env bash

docker run -d \
  -v $(pwd)/startup.sh:/startup.sh \
  -v $(pwd)/../src:/root/work/emotiq/src \
  -v $(pwd)/../var/local/etc:/root/work/emotiq/var/local/etc \
  -v $(pwd)/../etc:/root/work/emotiq/etc \
  -v $HOME/emotiq.conf:/root/.config/common-lisp/source-registry.conf.d/emotiq.conf \
  -v $HOME/quicklisp:/root/quicklisp \
  -v $HOME/.ccl-init.lisp:/root/.ccl-quicklisp \
  -p 65002:65002 \
  emotiq/ccl-dev:0.1 \
  /startup.sh
