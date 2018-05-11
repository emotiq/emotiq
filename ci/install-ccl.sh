#!/usr/bin/env bash

CCL_TAR_LINUX=https://github.com/Clozure/ccl/releases/download/v1.11.5/ccl-1.11.5-linuxx86.tar.gz
CCL_TAR_OSX=https://github.com/Clozure/ccl/releases/download/v1.11.5/ccl-1.11.5-darwinx86.tar.gz

mkdir -p $HOME/bin

uname_s=$(uname -s)
case ${uname_s} in
    Linux*)
        sudo apt-get update && sudo apt-get install -y \
          autoconf \
          build-essential \
          libcurl3-dev \
          libglib2.0-0 \
          curl \
          git-core \
          openssl \
          pgpgpg \
          gcc \
          make \
          g++ \
          gpgv2 \
          flex \
          bison \
          vim-nox \
          awscli \
          wget

        (cd $HOME && \
          wget -O /tmp/ccl.tgz $CCL_TAR_LINUX && \
          tar xvfz /tmp/ccl.tgz && \
          ln -s $HOME/ccl/lx86cl64 $HOME/bin/ccl)
        ;;
    Darwin*)
        brew update && brew install \
          wget
        (cd $HOME && \
          wget -O /tmp/ccl.tgz $CCL_TAR_OSX && \
          tar xvfz /tmp/ccl.tgz && \
          ln -s $HOME/ccl/dx86cl64 $HOME/bin/ccl)
        ;;
esac

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE=${DIR}/..

cp $BASE/ci/ccl-wrapper.sh $HOME/bin/ros

mkdir -p ~/.config/common-lisp/source-registry.conf.d/
echo "(:tree \"$BASE\")" >~/.config/common-lisp/source-registry.conf.d/emotiq.conf

echo "Installing Quicklisp"
wget -O /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
$HOME/bin/ros -l /tmp/quicklisp.lisp -e '(quicklisp-quickstart:install)'

cat >~/.ccl-init.lisp <<EOF
  ;;; The following lines added by ql:add-to-init-file:
  #-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
EOF

$HOME/bin/ros -e '(format t "~&~A ~A up and running! (ASDF ~A)~2%"
                (lisp-implementation-type)
                (lisp-implementation-version)
                #+asdf(asdf:asdf-version) #-asdf "not required")' || exit 1
