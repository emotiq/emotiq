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

emotiqfiles=emotiq emotiq.bash
production_dir=${prefix}/production

uname_s=$(uname -s)
case ${uname_s} in
    Linux*)
        MAKETARGET=makefile.linux
	deliveryscript=deliv-linux.bash
	arch=linux
	libs="libgmp.so libgmp.so.10 libgmp.so.10.3.2 libLispPBCIntf.so libpbc.so libpbc.so.1 libpbc.so.1.0.0"
	gmpflags=
	version=`/bin/date/date --iso-8601=seconds | tr [:] [_]`
        echo Using ${MAKETARGET}
        ;;
    Darwin*)
        MAKETARGET=makefile.osx
	deliveryscript=deliv-macos.bash
	arch=macos
	libs="libLispPBCIntf.dylib libgmp.10.dylib libgmp.dylib libpbc.1.dylib libpbc.dylib"
	gmpflags=--host=core2-apple-darwin17.5.0
	version=`/bin/date "+%Y%m%d%H%M%S"`
        echo Using ${MAKETARGET}
        ;;
    *)
        MAKETARGET=makefile.linux
        echo Unknown OS \"$(uname_s)\" -- defaulting to Linux Makefile

        ;;
esac

