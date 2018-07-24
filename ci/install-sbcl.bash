#!/usr/bin/env bash

version=${SBCL_VERSION:-1.4.8}

install_sbcl_macos() {
  wget -O /tmp/sbcl.bz2 https://github.com/emotiq/sbcl-binaries/releases/download/${version}/sbcl-${version}-x86-64-darwin-binary.tar.bz2
  (cd /tmp && \
    tar xfj sbcl.bz2 && \
    cd sbcl-${version}-x86-64-darwin && \
    ./install.sh
  )
}

install_sbcl_linux() {
  sudo apt-get update && sudo apt-get install -y \
    build-essential \
    wget
  wget -O /tmp/sbcl.bz2 https://github.com/emotiq/sbcl-binaries/releases/download/${version}/sbcl-${version}-x86-64-linux-binary.tar.bz2
  (cd /tmp && \
    tar xfj sbcl.bz2 && \
    cd sbcl-${version}-x86-64-linux && \
    sudo ./install.sh
  )
}

uname_s=$(uname -s)
case ${uname_s} in
    Linux*)
        install_sbcl_linux
        ;;
    Darwin*)
        install_sbcl_macos
        ;;
esac
