# Emotiq

This source tree constitutes the public Emotiq source repository from
<https://github.com/emotiq/emotiq>.

We intend to do our work transparently and openly in full public view
to this repository.

We assert an MIT license over this source aggregation; see
<https://github.com/emotiq/emotiq/blob/master/LICENSE.txt>.

## Developer installation instructions to test the basic EMOTIQ-TESTS system

This is a predominantly Common Lisp code base.

We aim to work on as many ANSI implementations as possible.

For the development of `testnet` we are targeting the commercial
LispWorks Pro 7.1 implementation but we ensure our code runs under
`ccl`.  We use LispWorks Pro to create the binaries we distribute, but
our we intend our full functionality to be always be available by
running on source with an arbitrary Common Lisp implementation to the
extent we have the resources for such an effort.

### Tell ASDF where to find the Emotiq systems

First, place a copy of this repository somewhere locally on your
machine.  We refer to that location as `~/work/emotiq/` in the
following instructions, so please adjust if you have your local copy
of the repository in a different location.

Configure ASDF to search `~/work/emotiq/` at Lisp boot by copying
<file:etc/emotiq.conf> to
`~/.config/common-lisp/source-registry.conf.d/`, creating the
destination directory if it doesn't already exist.

The following REPL command issued from the top-level source directory
will both create the directory and copy the file on operating systems
running some version of *NIX:

    (uiop:run-program "mkdir -p ~/.config/common-lisp/source-registry.conf.d/ && cp ~/work/emotiq/etc/emotiq.conf ~/.config/common-lisp/source-registry.conf.d/")


If you are under Windows or have placed your copy of this source tree
in a different location on the file-system, you will have to perform
the corresponding actions manually as per your local OS conventions.

Once the ASDF configuration has been edited correctly, one should be
able to verify that things are working via:

    (asdf:system-source-directory :emotiq)

### Configure Quicklisp

If the Quicklisp package manager is not locally available to your Lisp
implementation, first download and install it via the instructions
available at <https://www.quicklisp.org/beta/>.

For more exact versioning of our dependencies, we modify the standard
usage of Quicklisp in the following manner:

1.  We use an exact version of the `quicklisp` Quicklisp distribution,
    currently `quicklisp 2018-01-31`.
    
2.  We package dependencies not available in the `quicklisp`
    distribution within our own Quicklisp distribution named `emotiq`.
    We set the priority of the `emotiq` Quicklisp distribution higher
    than the `quicklisp` distribution which allows us to "patch"
    `quicklisp` dependencies as needed.

To effect the configuration of this setup, execute the forms in
`etc/setup-emotiq-quicklisp.lisp` after installing Quicklisp via:

    (load (asdf:system-relative-pathname :emotiq "../etc/setup-emotiq-quicklisp.lisp"))

### Building the native libraries required by CRYPTO-PAIRINGS

Currently, we have a dependency on a C library to do our pair based
curve (PBC) cryptography, which in turn depends on GMP library.

We have a separate repository
[emotiq-external-libs](https://github.com/emotiq/emotiq-external-libs),
where we build these libraries and store compiled version in the
GitHub Releases page.

To make access these libraries from Lisp code we have a glue library
libLispPBCIntf source of which is located as `src/Crypto/PBC-Intf`
directory.

The library currently only builds on Linux/MacOS.  It requires a
development tool-chain to be in place.

Once these tools are installed so that they may be invoked from a
shell, the script in `etc/build-crypto-pairings.bash` can be used to
drive the build. This script downloads compiled versions of external
libraries and builds the glue library. The results of that script are
created under a `var/` subdirectory.

Particular version of `emotiq-external-libs` binaries can be selected
by the variable `EXTERNAL_LIBS_VERSION` in the script
`etc/build-crypto-pairings.bash`, which should contain a `tag` of
release of `emotiq-external-libs` from
[Releases](https://github.com/emotiq/emotiq-external-libs/releases)

As a convenience, loading the ASDF definition for `crypto-pairings`
will attempt to run the script to create the native libraries.  If one
is updating this tree from a previous version, one may explicitly have
to force the asdf `prepare-op` via so:

    (asdf:make :crypto-pairings)
    
# Running 

After Quicklisp has been installed and configured, then issuing

    (ql:quickload :emotiq/sim)

will download all the dependencies needed by the tests gathered into
the `emotiq/sim` ASDF system.

To run the single node simulator, see the instructions in
<file:src/simulation.md>.

We have many ASDF descriptions within this repository whose
dependencies may need to be satisfied by via `ql:quickload`.

Currently we are working many systems simultaneously, most noteworthy
among them being the work in the `cosi-bls` system.

If in exploring the code one finds a missing dependency, say for the
system `cosi-bls`, a simple

    (ql:quickload :cosi-bls)

should satisfy the dependencies.  (TODO: `cl:restart` for missing
dependencies).

# Test 

To Evaluate form to test, which also loads, the system:

    (asdf:test-system :emotiq)

At end you should see a result like

    Unit Test Summary
     | 12 assertions total
     | 12 passed
     | 0 failed
     | 0 execution errors
     | 0 missing tests

The counts of assertions/passed should go up over time, and should
stay equal, with other counts staying zero.


All pushed to the source tree result in "Continuous Integration" build
from Travis CI: <https://travis-ci.org/emotiq/emotiq>.

The description of test coverage is contained in the Travis CI
artifact at <https://github.com/emotiq/emotiq/blob/dev/.travis.yml>.

# Colophon

    Copyright (c) 2018 Emotiq AG
    Created: 20-FEB-2018
    Revised: <2018-05-11 Fri 14:17Z>
