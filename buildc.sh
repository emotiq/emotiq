set -x
BASE=$PWD
# where the tarballs should be
TARDIR=${BASE}/src/Crypto/PBC-Intf
# pbc intf files are in the same place
PBCINTF=${TARDIR}
GMP=gmp-6.1.2.tar.bz2
PBC=pbc-0.5.14.tar

MAKETARGET=makefile.linux
#MAKETARGET=makefile.osx

# where make install will install stuff
DEST=${BASE}
PBCDIR=${TARDIR}/pbc-0.5.14
GMPDIR=${TARDIR}/gmp-6.1.2

LIB=${BASE}/lib
INC=${BASE}/include

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

cd ${TARDIR}
tar xfj ${TARDIR}/${GMP}
cd ${GMPDIR}
./configure --prefix=${BASE}
make
make install

if [ ! -d ${LIB} ]
then
    echo the file ${LIB} does not exist, something went wrong during build of gmp
    exit 1
fi

if [ ! -d ${INC} ]
then
    echo the file /${INC}/ does not exist, something went wrong during build of gmp
    exit 1
fi

export CFLAGS=-I${INC}
export CPPFLAGS=-I${INC}
export CXXFLAGS=-I${INC}
export LDFLAGS=-L${LIB}

cd ${TARDIR}
tar xf ${TARDIR}/${PBC}
cd ${PBCDIR}
./configure --prefix=${BASE}
make
make install

# finally, build libLispPBCIntf.so
if [ ! -d ${PBCINTF} ]
then
    echo the file /${PBCINTF}/ does not exist
    exit 1
fi
cd ${PBCINTF}
make --makefile=${MAKETARGET}
