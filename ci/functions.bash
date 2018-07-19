#!/usr/bin/env bash

function lisp_for_ci () {
    implementation=$1
    lisp_cli=
    case ${implementation} in
        lispworks*)
            lisp_cli="lwpro"
            ;;
        ccl*)
            lisp_cli="ccl"
            ;;
        sbcl*)
            lisp_cli="sbcl --disable-debugger"
            ;;
        *)
            # Fallback to ccl
            lisp_cli="ccl"
    esac
    echo $lisp_cli
}

function lisp_exec () {
    lisp=${lisp:-$(lisp_for_ci $LISP)}

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
