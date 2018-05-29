#!/usr/bin/env bash
set -x

KIND=${1:-production}
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

${DIR}/build-crypto-pairings.bash MAKESUFFIX=${makesuffix}

etc=${BASE}/etc
etcdeliver=${etc}/deliver

# where make install will install stuff
prefix=${var}/local
lib=${prefix}/lib

production_dir=${prefix}/production

date_now=`/bin/date "+%Y%m%d%H%M%S"`
hash=$(git rev-parse --short $(git log -1 --pretty=format:"%H"))
version="${date_now}-${hash}-${arch}"
echo -n $version > ${production_dir}/version.txt

target_dir=emotiq-${version}-${arch}
mkdir -p ${production_dir}/${target_dir}

case ${uname_s} in
  Linux*)
    libs="libgmp.so libgmp.so.10 libgmp.so.10.3.2 libLispPBCIntf.so libpbc.so libpbc.so.1 libpbc.so.1.0.0"
    ;;
  Darwin*)
    libs="libLispPBCIntf.dylib libgmp.10.dylib libgmp.dylib libpbc.1.dylib libpbc.dylib"
    ;;
esac

#
# Build binary
#
pushd ${production_dir}/${target_dir}
${etcdeliver}/${deliveryscript} ${KIND}
if [ $? -ne 0 ] ; then
  echo 'Build failure!'
  exit 127
fi
popd

# we use tar to preserve hard links (this can be rewritten)
pushd ${lib}
tar cf - ${libs} | tar xfC - ${production_dir}/${target_dir}
cp ${etc}/run-emotiq.bash ${production_dir}/${target_dir}/emotiq.bash
