#!/usr/bin/env bash -x

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

VAR_DIR=../../var/local/lib
PROD_DIR=../../var/local/production-linux

LWPRO=${LWPRO:-/usr/local/lib64/LispWorks/lispworks-7-1-0-amd64-linux}

"${LWPRO}" -build "${DIR}/../../src/deliver.lisp"

mkdir -p ${PROD_DIR}
mv emotiq ${PROD_DIR}
cp ${VAR_DIR}/*.so* ${PROD_DIR}


