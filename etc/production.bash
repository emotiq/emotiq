#!/usr/bin/env bash
#

# this script is for building the 'production' version in ../var/local/production-{linux,macos}

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
# here
etc=${BASE}/etc
etcdeliver=${etc}/deliver

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

version=`/bin/date "+%Y%m%d%H%M%S"`

uname_s=$(uname -s)
case ${uname_s} in
    Linux*)
	deliveryscript=deliv-linux.bash
	arch=linux
	gmpflags=
        echo Using ${MAKETARGET}
        ;;
    Darwin*)
	deliveryscript=deliv-macos.bash
	arch=macos
	gmpflags=--host=core2-apple-darwin17.5.0
        echo Using ${MAKETARGET}
        ;;
    *)
        echo Unknown OS \"$(uname_s)\" -- defaulting to Linux Makefile

        ;;
esac

tar_dir=${prefix}/production
tdir=emtq-${version}-${arch}
production_dir=${tar_dir}/${tdir}
mkdir -p ${production_dir}

lib_dir=${prefix}/lib
mkdir -p ${lib_dir}

# file names for tar
emotiqfiles="${tdir}/emotiq ${tdir}/emotiq.bash"
case ${uname_s} in
    Linux*)
	libswoprefix="libgmp.so libgmp.so.10 libgmp.so.10.3.2 libLispPBCIntf.so libpbc.so libpbc.so.1 libpbc.so.1.0.0"
	libs="${tdir}/libgmp.so ${tdir}/libgmp.so.10 ${tdir}/libgmp.so.10.3.2 ${tdir}/libLispPBCIntf.so ${tdir}/libpbc.so ${tdir}/libpbc.so.1 ${tdir}/libpbc.so.1.0.0"
        ;;
    Darwin*)
	libswoprefix="libLispPBCIntf.dylib libgmp.10.dylib libgmp.dylib libpbc.1.dylib libpbc.dylib"
	libs="${tdir}/libLispPBCIntf.dylib ${tdir}/libgmp.10.dylib ${tdir}/libgmp.dylib ${tdir}/libpbc.1.dylib ${tdir}/libpbc.dylib"
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

# PBC depends on GMP, so build GMP first

mkdir -p ${src}

cd ${src} \
    && tar -xjv -f ${gmp_tbz} \
    && cd ${gmp} \
    && ./configure --prefix=${prefix} \
    && make \
    && make install

if [ ! -d ${lib_dir} ]; then
    echo the directory ${lib_dir} does not exist, something went wrong during build of gmp
    exit 1
fi

if [ ! -d ${inc} ]; then
    echo the directory /${inc}/ does not exist, something went wrong during build of gmp
    exit 1
fi

export CFLAGS=-I${inc}
export CPPFLAGS=-I${inc}
export CXXFLAGS=-I${inc}
export LDFLAGS=-L${lib_dir}

cd ${src} \
    && tar -xv -f ${pbc_tar} \
    && cd ${pbc} \
    && ./configure ${gmpflags} --prefix=${prefix} \
    && make \
    && make install

cd ${pbcintf} && \
    make --makefile=makefile.${arch} PREFIX=${prefix}

cd ${etcdeliver}  # redundant?
bash ${etcdeliver}/${deliveryscript}

mv emotiq ${production_dir}
# we use tar to preserve hard links (can this be rewritten?)
cd ${lib_dir}
tar cf libs.tar ${libswoprefix}
mv ${lib_dir}/libs.tar ${production_dir}

cd ${production_dir}
tar xf libs.tar

cp ${etc}/emotiq.bash.${arch} ${production_dir}/emotiq.bash
# now, all dll's and emotiq are in ${production_dir}

tar cfj emotiq-${version}-${arch}.bz2 ${emotiqfiles} ${libs}
# remove libs to avoid possibly incorrect loading
# no, don't remove, rm -rf ${lib_dir}

mv emotiq-${version}-${arch}.bz2 ${tar_dir}
rm -rf ${production_dir}
# leaving only ${tar_dir} containing the bz2.
