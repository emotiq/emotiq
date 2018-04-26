#!/usr/bin/env bash
#
#  Assuming the proper development tools are in place, make the
#  libraries needed, and produce a shared object suitable for loading
#  the code for Pair Based Curves (PBC).
#
# Linux
#   apt-get install gcc make g++ flex bison
# MacOS
#   XCode needs to be installed


# debug
set -x

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

BASE=${DIR}/..
VAR=${BASE}/var


uname_s=$(uname -s)
case ${uname_s} in
    Linux*)
        echo Building for Linux
        libs_url=https://github.com/emotiq/emotiq-external-libs/releases/download/release-0.1/libLispPBCIntf-linux.tgz
        ;;
    Darwin*)
        echo Building for macOS
        libs_url=https://github.com/emotiq/emotiq-external-libs/releases/download/release-0.1/libLispPBCIntf-osx.tgz
        ;;
    *)
        maketarget=makefile.linux
        echo Unknown OS \"$(uname_s)\" -- defaulting to Linux Makefile
        exit 127
        ;;
esac

mkdir -p ${VAR}/local

(cd ${VAR}/local && wget -O - ${libs_url} | tar xvfz -)
