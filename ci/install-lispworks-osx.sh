#!/usr/bin/env bash
#
# Assumed we have LispWorks original files in local folder like this:
#     ./install.sh
#     ./lw71-amd64-linux.tar.gz
#     ./patches/lisp-memory-copy-32-chunks.64ufasl
#     ./patches/replace-i-vectors.64ufasl
#
# Also, needed license serial and keys inn environment
#
#     export LISPWORKS_SERIAL={hidden}
#     export LISPWORKS_LICENSE=[hidden]
#
set -x

FILE="/tmp/."`/bin/date '+%d%m%y'`"lispworks"`id -u`
touch $FILE
chmod 600 $FILE

PREFIX=$HOME
VERBOSE=0
FILELIST=
LWTAR=lw71-amd64-macos-patched.tar.gz
LWLICFILE=lib/7-1-0-0/config/lwlicense

while test $# -gt 0; do
  case "$1" in
    --prefix)
      PREFIX="$2"
      shift
      shift
      ;;
    -v)
      VERBOSE=1
      shift
      ;;
    *)
      break
      ;;
  esac
done

if test $# -ne 0; then
  echo "Usage: $0 [--prefix install-directory] [--excludedocs]"
elif test ! -f "/tmp/."`/bin/date '+%d%m%y'`"lispworks"`id -u`; then
  echo "**********************************************************"
  echo "* Please run lwl-license.sh before installing LispWorks. *"
  echo "**********************************************************"
else
  echo Installing LispWorks from `pwd` in $PREFIX

  brew update && brew install \
    awscli \
    gpg2 \
    wget

  aws s3 cp $LISPWORKS_BASE_URI/$LWTAR.gpg /tmp/$LWTAR.gpg

  if test ! -f /tmp/$LWTAR.gpg; then
    echo "Failure retrieving s3://emotiq-ci-supplements/lispworks/$LWTAR.gpg"
    exit 127
  fi

  gpg -d --passphrase $LISPWORKS_TAR_KEY --batch /tmp/$LWTAR.gpg > /tmp/$LWTAR
  if test ! -f /tmp/$LWTAR; then
    echo "Failure decrypting $LWTAR"
    exit 127
  fi

  if mkdir -p $PREFIX; then
    FILELIST=/tmp/lw-files-$$
    if test $VERBOSE = 1; then
      gzip -d < /tmp/$LWTAR | ( cd $PREFIX; tar xfv - | tee $FILELIST)
    else
      gzip -d < /tmp/$LWTAR | ( cd $PREFIX; tar xfv - > $FILELIST)
    fi
#    touch $PREFIX/$LWLICFILE
#    chmod a+w $PREFIX/$LWLICFILE
#    LWFILE="/tmp/."`/bin/date '+%d%m%y'`"lispworks"`id -u`
#    rm "$LWFILE"
    UIDGID=`id -u`:`id -g`
    for FILE in `cat $FILELIST`; do chown $UIDGID $PREFIX/$FILE; done
    rm $FILELIST
  fi
fi

"$PREFIX/LispWorks 7.1 (64-bit)/LispWorks (64-bit).app/Contents/MacOS/lispworks-7-1-0-amd64-darwin" --lwlicenseserial $LISPWORKS_SERIAL_OSX --lwlicensekey $LISPWORKS_LICENSE_OSX

cat >/tmp/resave.lisp <<EOF
(in-package "CL-USER")
(load-all-patches)
(save-image "~/bin/lwpro"
            :console t
            :multiprocessing t
            :environment nil)
EOF

mkdir -p ~/bin

"$PREFIX/LispWorks 7.1 (64-bit)/LispWorks (64-bit).app/Contents/MacOS/lispworks-7-1-0-amd64-darwin" -build /tmp/resave.lisp
cp ci/lwpro-wrapper.sh ~/bin/ros
chmod +x ~/bin/ros

# Setup ASDF searh path

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE=${DIR}/..

mkdir -p ~/.config/common-lisp/source-registry.conf.d/
echo "(:tree \"$BASE\")" >~/.config/common-lisp/source-registry.conf.d/emotiq.conf

echo "Installing Quicklisp"
wget -O /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
~/bin/ros -l /tmp/quicklisp.lisp -e '(quicklisp-quickstart:install)'

cat >~/.lispworks <<EOF
  ;;; The following lines added by ql:add-to-init-file:
  #-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
EOF

~/bin/ros -e '(format t "~&~A ~A up and running! (ASDF ~A)~2%"
                (lisp-implementation-type)
                (lisp-implementation-version)
                #+asdf(asdf:asdf-version) #-asdf "not required")' || exit 1


# ~/bin/ros -l ci/deps.lisp
