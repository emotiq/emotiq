#!/usr/bin/env bash
#
# To invoke outside one may set set the environment variables
#
# lisp
#   Lisp executable to use to invoke test harness.
#
# systems
#   Whitespace separated list of systems to test.
#
#
# For example the following entry (make sure that lwm is on your PATH)
#
# systems=":cosi-bls-tests :crypto-pairings :gossip-tests :emotiq/wallet" bash lw-test-harness.bash
#
# would invoke the test harness using `lwm` on the `gossip-tests`,
# `cosi-bls`, `crypto-pairings` and `emotiq/wallet` systems.
#
# BUGS
#
#  This version is for lispworks only, at the command line and uses
#  single-dash options (e.g. `-eval`).
#  See test-harness.bash for how to run this in CI (esp. with LW)

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# The command to run a Lisp test form:

lisp="/Applications/LispWorks\ 7.1\ (64-bit)/LispWorks\ (64-bit).app/Contents/MacOS/lispworks-7-1-0-amd64-darwin"

# The default whitespace list of systems to test
systems=${systems:-":gossip-tests
                    :crypto-pairings/t
                    :core-crypto
                    :emotiq/wallet"}

echo Test harness invoked using implementation: ${lisp}
echo $(${lisp} -eval '(format t "~&~a ~a~&" (lisp-implementation-type)(lisp-implementation-version))(uiop:quit 0)' < /dev/null)
# Sneaky pete exception for PROVE which needs a special ASDF syntax definition
echo $(${lisp} -eval '(ql:quickload :prove)(uiop:quit 0)' < /dev/null)

for system in ${systems}; do
    rm -rf ~/.cache/common-lisp/
    echo "==> ASDF:TEST-SYSTEM invoked on ${system}..."
    ${lisp} \
	-init - \
	-load "~/quicklisp/setup"\
        -load ${DIR}/lw-test-harness.lisp \
        -eval "(test-harness:test-system ${system})"
    status=$?
    if [[ $status -ne 0 ]] ; then
        echo "!== ASDF:TEST-SYSTEM failed for ${system}"
        exit $status
    fi
    echo "<== ASDF:TEST-SYSTEM on ${system} succeeded."
done
