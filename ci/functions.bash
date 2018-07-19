#!/usr/bin/env bash

lisp=${lisp:-'ccl'}

function lisp_exec () {
    tmpfile=$(mktemp /tmp/script-exec.lisp-XXXXXX)
    while test $# -gt 0; do
        case "$1" in
            --eval|-e)
                echo "$2" >> $tmpfile
                shift
                shift
                ;;
            --load|-l)
                echo "(load \"$2\")" >> $tmpfile
                shift
                shift
                ;;
            *)
                echo "Unknown argument $1!"
                exit 1
                ;;
        esac
    done
    echo "(uiop:quit 0)" >> $tmpfile
    ${lisp} <$tmpfile
}

function lisp_for_ci () {
    implementation=$1
    lisp_cli=
    case ${implementation} in
        lispworks*)
            lisp_cli="$HOME/bin/lwpro"
            ;;
        ccl*)
            lisp_cli="$HOME/bin/ccl"
            ;;
        sbcl*)
            lisp_cli="/usr/local/bin/sbcl --disable-debugger"
            ;;
    esac
    echo $lisp_cli
}
