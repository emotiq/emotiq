#!/usr/bin/env bash

install_sbcl_brew() {
  brew update && brew install sbcl
}

install_sbcl_linux() {
  sudo apt-get update && apt-get install -y \
    build-essential \
    wget
  wget -O /tmp/sbcl.bz2 http://prdownloads.sourceforge.net/sbcl/sbcl-1.4.7-x86-64-linux-binary.tar.bz2
  (cd /tmp && \
    tar xvfj sbcl.bz2 && \
    cd sbcl-1.4.7-x86-64-linux && \
    sudo ./install.sh
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
