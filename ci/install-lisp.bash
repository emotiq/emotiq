#!/usr/bin/env bash

uname_s=$(uname -s)
case ${uname_s} in
    Linux*)
        OS_NAME=linux
        ;;
    Darwin*)
        OS_NAME=osx
        ;;
    *)
        echo Unknown OS \"$(uname_s)\". Terminating...
        exit 127
        ;;
esac

case ${LISP} in
    lispworks*)
        ci/install-lispworks-$OS_NAME.bash
        rcfile="$HOME/.lispworks"
        ;;
    ccl*)
        ci/install-ccl.bash
        rcfile="$HOME/.ccl-init.lisp"
        ;;
    sbcl*)
        ci/install-sbcl.bash
        rcfile="$HOME/.sbclrc"
        ;;
esac

mkdir -p $HOME/bin

# Setup ASDF searh path

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE=${DIR}/..

mkdir -p ~/.config/common-lisp/source-registry.conf.d/
echo "(:tree \"$BASE\")" >~/.config/common-lisp/source-registry.conf.d/emotiq.conf

echo "Installing Quicklisp"
wget -O /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
$DIR/lisp-wrapper.bash -l /tmp/quicklisp.lisp -e '(quicklisp-quickstart:install)'

cat >$rcfile <<EOF
  ;;; The following lines added by ql:add-to-init-file:
  #-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
EOF

if [[ $LISP = lispworks* ]] ; then
  echo '(setf system:*sg-default-size* 32000)' >> $rcfile
fi

$DIR/lisp-wrapper.bash -e '(format t "~&~A ~A up and running! (ASDF ~A)~2%"
                (lisp-implementation-type)
                (lisp-implementation-version)
                #+asdf(asdf:asdf-version) #-asdf "not required")' || exit 1
