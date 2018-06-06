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
        ci/install-lispworks-$OS_NAME.sh
        rcfile="$HOME/.lispworks"
        ;;
    ccl*)
        ci/install-ccl.sh
        rcfile="$HOME/.ccl-init.lisp"
        ;;
    sbcl*)
        ci/install-sbcl.sh
        rcfile="$HOME/.sbclrc"
        ;;
esac

mkdir -p $HOME/bin
cp ci/lisp-wrapper.sh $HOME/bin
chmod +x $HOME/bin/lisp-wrapper.sh

# Setup ASDF searh path

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE=${DIR}/..

mkdir -p ~/.config/common-lisp/source-registry.conf.d/
echo "(:tree \"$BASE\")" >~/.config/common-lisp/source-registry.conf.d/emotiq.conf

echo "Installing Quicklisp"
wget -O /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
$HOME/bin/lisp-wrapper.sh -l /tmp/quicklisp.lisp -e '(quicklisp-quickstart:install)'

cat >$rcfile <<EOF
  ;;; The following lines added by ql:add-to-init-file:
  #-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
EOF

$HOME/bin/lisp-wrapper.sh -e '(format t "~&~A ~A up and running! (ASDF ~A)~2%"
                (lisp-implementation-type)
                (lisp-implementation-version)
                #+asdf(asdf:asdf-version) #-asdf "not required")' || exit 1
