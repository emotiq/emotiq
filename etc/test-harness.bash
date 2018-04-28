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
#   lisp=ccl systems=:gossip-test bash test-harness.bash
#
# would invoke the test harness using `ccl` on the `gossip-test`
# system.
DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# The command to run a Lisp test form:
lisp=${lisp:-'ros'}

# whitespace list of systems to test
systems=${systems:-":emotiq/blockchain
                    :crypto-pairings/t
                    :core-crypto"}

echo Test harness invoked using implementation:
echo $(${lisp} --eval '(format t "~&~a~&~a~&" (lisp-implementation-type)(lisp-implementation-version))(uiop:quit 0)')

for system in ${systems}; do
    rm -rf ~/.cache/common-lisp/
    echo "==>Invoking ASDF:TEST-SYSTEM on ${system}..."
    ${lisp} \
        --load ${DIR}/test-harness.lisp \
        --eval "(test-harness:test-system ${system})"
    status=$?
    if [[ $status -ne 0 ]] ; then
        echo "ASDF:TEST-SYSTEM failed for ${system}"
        exit $status
    fi
    echo "<==ASDF:TEST-SYSTEM on ${system} succeeded."
done
                                      
