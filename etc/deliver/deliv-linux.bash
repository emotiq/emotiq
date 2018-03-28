#!/usr/bin/env bash -x

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

LWPRO=${LWPRO:-/usr/local/lib64/LispWorks/lispworks-7-1-0-amd64-linux}

exec "${LWPRO}" -build "${DIR}/../../src/deliver.lisp"


