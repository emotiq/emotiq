#!/usr/bin/env bash
#
# Convert the contents of the environment variable LISP to the local
# path an executable Lisp image, then use it to specify the
# implementation to use to run the tests.

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

. ${DIR}/lisp-wrapper-location.bash
lisp=$(lisp_for_ci ${LISP})

lisp=$lisp ${DIR}/../etc/test-harness.bash 


       
