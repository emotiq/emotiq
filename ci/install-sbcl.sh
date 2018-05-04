#!/usr/bin/env bash

install_sbcl_brew() {
  wget -O /tmp/sbcl.bz2 https://github.com/emotiq/sbcl-binaries/releases/download/1.4.7/sbcl-1.4.7-x86-64-darwin-binary.tar.bz2
  (cd /tmp && \
    tar xfj sbcl.bz2 && \
    cd sbcl-1.4.7-x86-64-darwin && \
    ./install.sh
  )
}

install_sbcl_linux() {
  sudo apt-get update && apt-get install -y \
    build-essential \
    wget
  wget -O /tmp/sbcl.bz2 https://github.com/emotiq/sbcl-binaries/releases/download/1.4.7/sbcl-1.4.7-x86-64-linux-binary.tar.bz2
  (cd /tmp && \
    tar xfj sbcl.bz2 && \
    cd sbcl-1.4.7-x86-64-linux && \
    ./install.sh
  )
}

uname_s=$(uname -s)
case ${uname_s} in
    Linux*)
        install_sbcl_linux
        ;;
    Darwin*)
        install_sbcl_brew
        ;;
esac
