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


FILE="/tmp/."`/bin/date '+%d%m%y'`"lispworks"`id -u`
touch $FILE
chmod 600 $FILE

PREFIX=~/LispWorks
VERBOSE=0
FILELIST=
LWTAR=lw71-amd64-linux-patched.tar.gz
DOCTAR=lwdoc71-x86-linux.tar.gz
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
  apt-get update && sudo apt-get install -y \
    autoconf \
    build-essential \
    libcurl3-dev \
    libglib2.0-0 \
    libgtk2.0-0 \
    curl \
    git-core \
    openssl \
    pgpgpg \
    wget \
    bash \
    gcc \
    make \
    g++ \
    gpgv2 \
    flex \
    bison \
    vim-nox \
    xorg \
    xvfb \
    xfonts-100dpi \
    xfonts-75dpi \
    xfonts-scalable \
    xfonts-cyrillic \
    awscli \
    wget
    
  aws s3 $LISPWORKS_BASE_URI/$LWTAR.gpg .

  if test ! -f $LWTAR.gpg; then
    echo "Failure retrieving s3://emotiq-ci-supplements/lispworks/$LWTAR.gpg"
    exit 127
  fi

  gpg -d --passphrase $LISPWORKS_TAR_KEY --batch lw71-amd64-linux-patched.tar.gz.gpg > lw71-amd64-linux-patched.tar.gz
  if test ! -f $LWTAR; then
    echo "Failure decrypting $LWTAR"
    exit 127
  fi

  if mkdir -p $PREFIX; then
    FILELIST=/tmp/lw-files-$$
    if test $VERBOSE = 1; then
      gzip -d < $LWTAR | ( cd $PREFIX; tar xfv - | tee $FILELIST)
    else
      gzip -d < $LWTAR | ( cd $PREFIX; tar xfv - > $FILELIST)
    fi
    touch $PREFIX/$LWLICFILE
    chmod a+w $PREFIX/$LWLICFILE
    LWFILE="/tmp/."`/bin/date '+%d%m%y'`"lispworks"`id -u`
    rm "$LWFILE"
    UIDGID=`id -u`:`id -g`
    for FILE in `cat $FILELIST`; do chown $UIDGID $PREFIX/$FILE; done
    rm $FILELIST
  fi
fi

$PREFIX/lispworks-7-1-0-amd64-linux --lwlicenseserial $LISPWORKS_SERIAL --lwlicensekey $LISPWORKS_LICENSE

cat >resave.lisp <<EOF
(in-package "CL-USER")
(load-all-patches)
(save-image "~/bin/lwpro"
            :console t
            :multiprocessing t
            :environment nil)
EOF

mkdir -p ~/bin

xvfb-run $PREFIX/lispworks-7-1-0-amd64-linux -build resave.lisp
