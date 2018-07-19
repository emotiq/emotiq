#!/usr/bin/env bash

version=${CCL_VERSION:-1.11.5}

CCL_TAR_LINUX=https://github.com/Clozure/ccl/releases/download/v${version}/ccl-${version}-linuxx86.tar.gz
CCL_TAR_OSX=https://github.com/Clozure/ccl/releases/download/v${version}/ccl-${version}-darwinx86.tar.gz

mkdir -p $HOME/bin

uname_s=$(uname -s)
case ${uname_s} in
    Linux*)
        sudo apt-get update && sudo apt-get install -y \
          autoconf \
          build-essential \
          libcurl3-dev \
          libglib2.0-0 \
          curl \
          git-core \
          openssl \
          pgpgpg \
          gcc \
          make \
          g++ \
          gpgv2 \
          flex \
          bison \
          vim-nox \
          awscli \
          wget

        (cd $HOME && \
          wget -O /tmp/ccl.tgz $CCL_TAR_LINUX && \
          tar xvfz /tmp/ccl.tgz && \
          ln -s $HOME/ccl/lx86cl64 $HOME/bin/ccl)
        ;;
    Darwin*)
        brew update && brew install \
          wget
        (cd $HOME && \
          wget -O /tmp/ccl.tgz $CCL_TAR_OSX && \
          tar xvfz /tmp/ccl.tgz && \
          ln -s $HOME/ccl/dx86cl64 $HOME/bin/ccl)
        ;;
esac
