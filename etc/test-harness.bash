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
# For example the following entry
#
#   lisp=ccl systems=":gossip-tests :cosi-bls" bash test-harness.bash
#
# would invoke the test harness using `ccl` on the `gossip-tests` and
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
lisp=${lisp:-'ccl'}

# systems=${systems:-$(${lisp} --noinit --load ${DIR}/parse-systems.lisp < /dev/null)}
systems=${systems:-":emotiq-test \
                    :gossip-test \
                    :crypto-pairings-test \
                    :emotiq-config-test \
                    :cosi-bls-test"}

echo Test harness invoked using implementation: ${lisp}
echo $(${lisp} --eval '(format t "~&~a ~a~&" (lisp-implementation-type)(lisp-implementation-version))(uiop:quit 0)' < /dev/null)

# Collect whitespace list of failed systems
failed=

for system in ${systems}; do
    rm -rf ~/.cache/common-lisp/
    echo "==> ASDF:TEST-SYSTEM invoked on ${system}..."
    ${lisp} \
        --eval "(ql:quickload :asdf-test-harness)" \
        --eval "(ql:quickload ${system})" \
        --eval "(asdf-test-harness:run-suite ${system})" < /dev/null
    status=$?
    if [[ $status -ne 0 ]] ; then
        echo "!== ASDF:TEST-SYSTEM failed for ${system}"
        failed="${failed} ${system}"
    fi
    echo "<== ASDF:TEST-SYSTEM on ${system} succeeded."
done

if [[ ! -z ${failed} ]]; then
    echo === Tests failed for ${failed}
    exit 1
fi

exit 0

                                
   
