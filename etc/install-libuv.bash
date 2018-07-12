#!/usr/bin/env bash
#

# debug
set -x

EXTERNAL_LIBS_VERSION=release-0.1.13

uname_s=$(uname -s)
case ${uname_s} in
    Linux*)
        LIBUV_URL=https://github.com/emotiq/emotiq-external-libs/releases/download/${EXTERNAL_LIBS_VERSION}/emotiq-libuv-linux.tgz
        SUDO=sudo
        ;;
    Darwin*)
        LIBUV_URL=https://github.com/emotiq/emotiq-external-libs/releases/download/${EXTERNAL_LIBS_VERSION}/emotiq-libuv-osx.tgz
        SUDO=
        ;;
    *)
        echo Unknown OS \"$(uname_s)\"
        exit 127
        ;;
esac

(cd /usr/local && curl -L ${LIBUV_URL} | ${SUDO} tar xvfz -)
