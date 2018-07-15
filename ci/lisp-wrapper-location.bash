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

