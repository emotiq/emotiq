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


# if the first arg $1 is "prod" then the production makefiles are used
# and the appropriate deliver/ script is used

# debug
set -x

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

BASE=${DIR}/..
tmp=${BASE}/tmp

# where the tarballs should be
CRYPTO=${BASE}/src/Crypto
dist=${CRYPTO}/PBC-Intf
# pbc intf files are in the same place
pbcintf=${dist}
gmp_tbz=${dist}/gmp-6.1.2.tar.bz2
pbc_tar=${dist}/pbc-0.5.14.tar

# where make install will install stuff
var=${BASE}/var
src=${BASE}/src
gmp=${src}/gmp-6.1.2
pbc=${src}/pbc-0.5.14
prefix=${var}/local


lib=${prefix}/lib
inc=${prefix}/include

#  ${deliver} will be used (at bottom) only if $1 == "prod"
uname_s=$(uname -s)
case ${uname_s} in
    Linux*)
	prod_dir=${prefix}/production-linux
	mkdir -p ${prod_dir}
        MAKETARGET=makefile.linux
	deliver=${DIR}/deliver/deliv-linux.bash
        echo Using ${MAKETARGET}
        ;;
    Darwin*)
	prod_dir=${prefix}/production-macos
	mkdir -p ${prod_dir}
        MAKETARGET=makefile.osx
	deliver=${DIR}/deliver/deliv-macos.bash
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

case $1 in
    prod)
	cd ${pbcintf} && \
	    make --makefile=${MAKETARGET}.production PREFIX=${prefix}
	cd ${DIR}
	bash ${deliver} ${prod_dir}
	# at this point, the 3 DLL's (hard linked with versioned DLL)
	# ... and emotiq (the delivered binary is sitting here (${DIR})
	# are sitting in ../var/local/lib and the delivered emotiq binary is in ../var/local/production-linux

	# copy *.so* into ../var/local/production-linux, preserving hard links (.e. use tar)
#	cd ${lib}
#	tar cf libs.tar *.so*
#	mv libs.tar ${prod_dir}
#	cd ${prod_dir}
#	tar xf libs.tar

#	# now emotiq (binary) and *.so* are in ../var/local/production-linux ( ${prod_dir} )

#	rm libs.tar
#	file * | grep 'symbolic link' | sed -e 's/:.*$//g' >links.txt
#	rm -f `echo links.txt`


# cd ${VAR_DIR}
#tar cf linux-emotiq.tar *.so* emotiq
#	mkdir -p ${tmp}#
#	mv ${lib}/linux-emotiq.tar ${tmp}
#	cd ${tmp}
	#	tar xf linux-emotiq.tar
	
        ;;
    *)
	cd ${pbcintf} && \
	    make --makefile=${MAKETARGET} PREFIX=${prefix}
        ;;
esac



