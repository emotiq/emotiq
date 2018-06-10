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
# For example
#
#   lisp=ccl systems=":gossip-test :cosi-bls" bash test-harness.bash
#
# would invoke the test harness using `ccl` on the `gossip-test` and
# `cosi-bls` systems.
#
# BUGS
#
#  LispWorks cannot directly be used due to the use of single dash
#  command line options (i.e. `-eval` instead of `--eval`).  Instead,
#  an invocation wrapper like Roswell must be used to invoke
#  LispWorks.
DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# The command to run a Lisp test form:
lisp=${lisp:-'ros'}

# The default whitespace list of systems to test
systems=${systems:-":emotiq/blockchain
                    :crypto-pairings/t
                    :core-crypto"}

echo Test harness invoked using implementation: ${lisp}
echo $(${lisp} --eval '(format t "~&~a ~a~&" (lisp-implementation-type)(lisp-implementation-version))(uiop:quit 0)' < /dev/null)

for system in ${systems}; do
    rm -rf ~/.cache/common-lisp/
    echo "==> ASDF:TEST-SYSTEM invoked on ${system}..."
    ${lisp} \
        --load ${DIR}/test-harness.lisp \
        --eval "(test-harness:test-system ${system})"
    status=$?
    if [[ $status -ne 0 ]] ; then
        echo "!== ASDF:TEST-SYSTEM failed for ${system}"
        exit $status
    fi
    echo "<== ASDF:TEST-SYSTEM on ${system} succeeded."
done
