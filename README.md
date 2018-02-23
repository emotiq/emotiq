# Emotiq

This source tree constitutes the public Emotiq source repository from
<https://github.com/emotiq/emotiq>.

We intend to do our work transparently and openly in full public view
to this repository.

We assert an MIT license over this source aggregation; see
<https://github.com/emotiq/emotiq/blob/master/LICENSE.txt>.

## Developer installation instructions to test the basic EMOTIQ-TESTS system

This is a predominantly Common Lisp code base.

We aim to work on as many ANSI implementation as possible.  We are
currently targeting the commercial Allegro Common Lisp 10.1
implementation but we also test our code with `sbcl-1.4.4` and
`ccl-1.11`.  Currently not all ASDF systems run cleanly outside of
`acl`.

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
in a different location on the filesystem, you will have to perform
the corresponding actions manually as per your local OS conventions.

Once the ASDF configuration has been edited correctly, one should be
able to verify that things are working via

    (asdf:system-source-directory :emotiq)

### Ensure Quicklisp dependencies have been satified

This only needs to be performed once per installation.

If Quicklisp is not locally available to your Lisp implementation,
first download and install it via the instructions available at
<https://www.quicklisp.org/beta/>.

After Quicklisp has been installed, then issuing 

    (ql:quickload :prove)
    (ql:quickload :emotiq/t)
    
will download all the dependencies needed by the tests gathered into
the `emotiq/t` system.

We have many ASDF descriptions within this repository whose
dependencies may need to be satisfied by via `ql:quickload` other than
`emotiq/t`.  

Currently we are working many systems simultaneously, most noteworthy
among them being the work in the `cosi` system.

If in exploring the code one finds a missing dependency, say for the
system `cosi`, a simple

    (ql:quickload :cosi)

should satisfy the dependencies.  (TODO: `cl:restart` for missing
dependencies).

### Evaluate form to test, which also loads, the system:

    (asdf:test-system :emotiq)

At end you should see a result like


    Load 1 ASDF system:
        emotiq/t
    ; Loading "emotiq/t"


    Running a test file '/Users/evenson/work/emotiq/src/t/base.lisp'
    ;Loading #P"/Users/evenson/work/emotiq/src/t/base.lisp"...
    1..1

      ✓ A successful test of compiling dependencies and passing a test.

    ✓ 1 test completed (0ms)

    Running a test file '/Users/evenson/work/emotiq/src/t/tests.lisp'
    ;Loading #P"/Users/evenson/work/emotiq/src/t/tests.lisp"...
    1..3

     emotiq:hex-string-to-octet-vector on "a0b1c2d3e4f56789"…
      ✓ octet-vector-p… 

      ✓ length octect-vector… 

      ✓ back-hex in emotiq:octet-vector-to-hex-string… 

    1..1

      ✓ emotiq:ovvref on emotiq:hex-string-to-octet-vector… 

     Testing crypto on "Rosetta code"
    1..2

      ✓ sha256-string… 

      ✓ digest length… 

    1..2

      ✓ Vector of digest… 

      ✓ Length of digest… 

    1..2

      ✓ sha3-512-string… 

      ✓ sha3-512-string digest length… 

    1..2

      ✓ sha3-512 vector comparison… 

      ✓ sha3-512 vector digest length… 

    ✓ 2 tests completed (0ms)

    Summary:
      All 2 files passed.


# Colophon
    
    Copyright (c) 2018 Emotiq AG
    Created: 20-FEB-2018
    Revised: <2018-02-23 Fri 16:09Z>
