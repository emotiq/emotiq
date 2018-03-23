set -x
BASE=$PWD
# where the tarballs should be
TARDIR=${BASE}/src/Crypto/PBC-Intf
GMP=gmp-6.1.2.tar.bz2
PBC=pbc-0.5.14.tar

LIB=${BASE}/lib
# where make install will root is tree
DEST=${LIB}
PBCDIR=${LIB}/pbc-0.5.14
GMPDIR=${LIB}/gmp-6.1.2

if [ ! -f ${TARDIR}/${GMP} ]
then
    echo the file ${TARDIR}/${GMP} does not exist
    exit 1
fi
if [ ! -f ${TARDIR}/${PBC} ]
then
    echo the file ${TARDIR}/${PBC} does not exist
    exit 1
fi
if [ -d ${LIB} ]
then
    rm -rf ${LIB}
fi

# PBC depends on GMP, so build GMP first

cd ${LIB}
tar xf ${TARDIR}/${GMP}
cd ${GMPDIR}
./configure --prefix=${BASE}
make
#make DESTDIR=${DEST} install
make install

if [ ! -d ${LIB} ]
then
    echo the file ${LIB} does not exist, something went wrong during build of gmp
    exit 1
fi

if [ ! -d ${LIB}/usr/local/lib ]
then
    echo the file ${LIB}/usr/local/lib does not exist, something went wrong during build of gmp
    exit 1
fi

mv ${LIB}/usr/local/lib/* ${LIB}
mv ${LIB}/usr/local/include ${LIB}

LDFLAGS="-L${LIB}"
export LIBRARY_PATH=${LIB}:$LIBRARY_PATH
export LD_LIBRARY_PATH=${LIB}:$LD_LIBRARY_PATH
export C_INCLUDE_PATH=${LIB}/include:$LD_LIBRARY_PATH
export C_INCLUDE_PATH=${LIB}/include:$LD_LIBRARY_PATH
echo paths /${LIBRARY_PATH}/ /$LD_LIBRARY_PATH}/

cd ${LIB}
tar xf ${TARDIR}/${PBC}
cd ${PBCDIR}
./configure --prefix=${BASE}
pwd
make
#make DESTDIR=${DEST} install
make install
