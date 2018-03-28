#!/usr/bin/env bash -x

# this has been tested only on 32-bit LWM
# Cocoa adds complications (see Delivery manual).  This binary appears not to need Cocoa, so things are simple.
# basically, this script calls LWM with two arguments '-build' and 'deliver.lisp'
# Change the pathname appropriately for 64-bit LWM

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

uname_s=$(uname -s)
case ${uname_s} in
    Linux*)
	LWPRO=${LWPRO:-/usr/local/lib64/LispWorks/lispworks-7-1-0-amd64-linux}
        ;;
    Darwin*)
	if [ -f '/Applications/LispWork\ 7.0\ \(32-bit\)/LispWorks\ \(32-bit\).app/Contents/MacOS/lispworks-7-0-0-x86-darwin' ]
	then
	    LWPRO_MACOS_64=${LWPRO_MACOS_64:-/Applications/LispWorks\ 7.1\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS/lispworks-7-1-0-amd64-darwin}
	    LWPRO=${LWPRO_MACOS_64}
	else
	    LWPRO_MACOS_32=${LWPRO_MACOS_32:-/Applications/LispWork\ 7.0\ \(32-bit\)/LispWorks\ \(32-bit\).app/Contents/MacOS/lispworks-7-0-0-x86-darwin}
	    LWPRO=${LWPRO_MACOS_32}
	fi
        ;;
    *)
	LWPRO=${LWPRO:-/usr/local/lib64/LispWorks/lispworks-7-1-0-amd64-linux}
        echo Unknown OS \"$(uname_s)\" -- defaulting to Linux Makefile

        ;;
esac

exec "${LWPRO}" -build "${DIR}/../src/deliver.lisp"


