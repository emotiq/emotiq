#!/usr/bin/env bash
# debug
set -x

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE=${DIR}/..
var=${BASE}/var

if [ ${var} ]
then
    rm -rf ${var}
fi

uname_s=$(uname -s)
case ${uname_s} in
    Linux*)
	deliveryscript=deliv-linux.bash
	makesuffix=
	arch=linux
        ;;
    Darwin*)
	deliveryscript=deliv-macos.bash
	makesuffix=".production"
	arch=macos
        ;;
esac

. ${DIR}/build-crypto-pairings.bash MAKESUFFIX=${makesuffix}

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
src=${var}/src
prefix=${var}/local
gmp=${src}/gmp-6.1.2
pbc=${src}/pbc-0.5.14

lib=${prefix}/lib
inc=${prefix}/include

version=`/bin/date "+%Y%m%d%H%M%S"`


tar_dir=${prefix}/production
tdir=emotiq-${version}-${arch}
production_dir=${tar_dir}/${tdir}
mkdir -p ${production_dir}

echo -n $version > ${tar_dir}/version.txt

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


cd ${etc}
. ${etcdeliver}/${deliveryscript}
if [ $? -ne 0 ] ; then
  echo 'Build failure!'
  exit 127
fi

mv ${etc}/emotiq ${production_dir}

# we use tar to preserve hard links (this can be rewritten)
cd ${lib_dir}
tar cf libs.tar ${libswoprefix}
mv ${lib_dir}/libs.tar ${production_dir}

cd ${production_dir}
tar xf libs.tar

cp ${etc}/emotiq.bash.${arch} ${production_dir}/emotiq.bash
# now, all dll's and emotiq are in ${production_dir}

# this seems stupid, but I don't know enough about tar's options to make it include the subdir name
cd ${tar_dir}
tar cfj emotiq-${version}-${arch}.tar.bz2 ${emotiqfiles} ${libs}
# remove libs to avoid possibly incorrect loading
# no, don't remove, rm -rf ${lib_dir}

rm -rf ${production_dir}
# leaving only ${tar_dir} containing the bz2.
