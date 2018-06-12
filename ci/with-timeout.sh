#!/usr/bin/env bash

case $(uname -s) in
    Linux*)
        timeout_cli=timeout
        ;;
    Darwin*)
        timeout_cli=gtimeout
        ;;
    CYGWIN_NT*)
        timeout_cli=timeout
        ;;
    *)
        echo Unknown OS \"$(uname_s)\". Terminating...
        exit 127
        ;;
esac

tmpfile=$(mktemp /tmp/with-timout.XXXXXX)
echo "#!/usr/bin/env bash" > $tmpfile
echo $* >> $tmpfile
chmod +x $tmpfile

exec ${timeout_cli} -k 11m 10m $tmpfile
