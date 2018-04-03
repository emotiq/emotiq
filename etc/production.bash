#!/usr/bin/env bash

# this script is for building the 'production' version in ../var/local/production-{linux,macos}

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

# where make install will install stuff
var=${BASE}/var
src=${var}/src
prefix=${var}/local
gmp=${src}/gmp-6.1.2
pbc=${src}/pbc-0.5.14

lib=${prefix}/lib
inc=${prefix}/include
etc=${BASE}/etc
delivery=${BASE}/etc/deliver

uname_s=$(uname -s)
#  ${deliveryscript} and ${prod_dir} will be used at the bottom, for production
case ${uname_s} in
    Linux*)
        MAKETARGET=makefile.linux
	deliveryscript=deliv-linux.bash
	prod_dir=${prefix}/production-linux
        echo Using ${MAKETARGET}
        ;;
    Darwin*)
        MAKETARGET=makefile.osx
	deliveryscript=deliv-macos.bash
	prod_dir=${prefix}/production-macos
        echo Using ${MAKETARGET}
        ;;
    *)
        MAKETARGET=makefile.linux
        echo Unknown OS \"$(uname_s)\" -- defaulting to Linux Makefile

        ;;
esac

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
    && ./configure --prefix=${prefix} \
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
    make --makefile=${MAKETARGET}.production PREFIX=${prefix}

cd ${delivery}  # redundant?
bash ${delivery}/${deliveryscript}

mkdir -p ${prod_dir}
mv emotiq ${prod_dir}
# < test >
# for testing - we must use tar to preserve hard links
cd ${lib}
tar cf libs.tar *
cd ${prod_dir}
mv ${lib}/libs.tar ${prod_dir}
tar xf libs.tar
# now, all dll's and emotiq are in ${prod_dir}
cp ${etc}/emotiq.bash ${prod_dir}
cd ${lib}
rm *
# < /test >

# cp ${VAR_DIR}/*.so* ${prod_dir}


