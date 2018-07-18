#!/usr/bin/env bash
#
#  Assuming the proper development tools are in place, make the
#  libraries needed, and produce a shared object suitable for loading
#  the code for High Performance Curve1174 and Ed3363.
#
# Linux
#   apt-get install gcc make g++ flex bison
# MacOS
#   XCode needs to be installed


# debug
set -x

EXTERNAL_LIBS_VERSION=release-0.1.13

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

BASE=${DIR}/..
var=${BASE}/var

uname_s=$(uname -s)
case ${uname_s} in
    Linux*)
        echo Building for Linux
        lib_suffix=linux
        maketarget=makefile.linux
        if [ "x${PENTIUM4}" == "xtrue" ] ; then
          EXTERNAL_LIBS_VERSION=release-0.1.8-p4-linux
        fi
        ;;
    Darwin*)
        echo Building for macOS
        lib_suffix=osx
        maketarget=makefile.macos
        ;;
    *)
        maketarget=makefile.linux
        echo Unknown OS \"$(uname_s)\" -- defaulting to Linux Makefile
        exit 127
        ;;
esac

libs_url=https://github.com/emotiq/emotiq-external-libs/releases/download/${EXTERNAL_LIBS_VERSION}/emotiq-external-libs-${lib_suffix}.tgz

mkdir -p ${var}/local/{lib,include}

prefix=${var}/local
lib=${prefix}/lib
inc=${prefix}/include
eccintf=${BASE}/src/Crypto/Ed3363-C-Code

export CFLAGS=-I${inc}
export CPPFLAGS=-I${inc}
export CXXFLAGS=-I${inc}
export LDFLAGS=-L${lib}

cd ${eccintf} && \
    make --makefile=${maketarget} PREFIX=${prefix}
