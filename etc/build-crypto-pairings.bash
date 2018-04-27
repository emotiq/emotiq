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
# where the tarballs should be
CRYPTO=${BASE}/src/Crypto
dist=${CRYPTO}/PBC-Intf
# pbc intf files are in the same place
pbcintf=${dist}
gmp_tbz=${dist}/gmp-6.1.2.tar.bz2
pbc_tar=${dist}/pbc-0.5.14.tar

uname_s=$(uname -s)
case ${uname_s} in
    Linux*)
	gmpflags=
        maketarget=makefile.linux
        echo Using ${maketarget}
        ;;
    Darwin*)
	gmpflags=--host=core2-apple-darwin17.5.0
        maketarget=makefile.osx${MAKESUFFIX}
        echo Using ${maketarget}
        ;;
    *)
        maketarget=makefile.linux
        echo Unknown OS \"$(uname_s)\" -- defaulting to Linux Makefile

        ;;
esac

# where make install will install stuff
var=${BASE}/var
src=${var}/src
prefix=${var}/local
gmp=${src}/gmp-6.1.2
pbc=${src}/pbc-0.5.14

lib=${prefix}/lib
inc=${prefix}/include

if [ ! -f ${gmp_tbz} ]
then
    echo the file ${gmp_tbz} does not exist
    exit 1
fi
if [ ! -f ${pbc_tar} ]
then
    echo the file ${pbc_tar} does not exist
    exit 1
fi

# Should not be necessary…
# if [ -d ${LIB} ]
# then
#     rm -rf ${LIB}
# fi

# PBC depends on GMP, so build GMP first

mkdir -p ${src}

cd ${src} \
    && tar -xjv -f ${gmp_tbz} \
    && cd ${gmp} \
    && ./configure ${gmpflags} --prefix=${prefix} \
    && make \
    && make install

if [ ! -d ${lib} ]; then
    echo the directory ${lib} does not exist, something went wrong during build of gmp
    exit 1
fi

if [ ! -d ${inc} ]; then
    echo the directory /${inc}/ does not exist, something went wrong during build of gmp
    exit 1
fi

export CFLAGS=-I${inc}
export CPPFLAGS=-I${inc}
export CXXFLAGS=-I${inc}
export LDFLAGS=-L${lib}

cd ${src} \
    && tar -xv -f ${pbc_tar} \
    && cd ${pbc} \
    && ./configure --prefix=${prefix} \
    && make \
    && make install

cd ${pbcintf} && \
    make --makefile=${maketarget} PREFIX=${prefix}


