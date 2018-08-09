#!/usr/bin/env bash
DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

lisp=${lisp:-lwpro}

function lisp_exec () {
    # We're running in the CI
    if [[ ${lisp} == "ci/lisp-wrapper.bash" ]]; then
        ${lisp} $*
    else
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
    fi
}

target_dir=${1:-/var/tmp/emotiq/etc.$$/}

echo Generating configuration artifacts in ${target_dir}

lisp_exec \
    --eval "(ql:quickload :emotiq/config/generate)" \
    --eval "(emotiq/config/generate:ensure-defaults :force t :for-purely-local t :destination \"${target_dir}\")"




