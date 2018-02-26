;;;; emotiq.asd

(defsystem "emotiq"
  :description "Emotiq"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on (#:ironclad #:bordeaux-threads)
  :serial t
  :perform (test-op (o s)
                    (uiop:symbol-call :lisp-unit :run-tests
                                      :all '#:emotiq-tests))
  :components ((:file "package")
               (:file "utilities")
               (:file "external")
               (:file "blockchain"))
  :in-order-to ((test-op (test-op "emotiq-tests"))))


#| Abbreviated instructions for a test build in the Lisp REPL:

0. [Something about assumption that Quicklisp has been set up....!] 

1. Tell ASDF where to find system: (Substitute as appropriate for "/path/to/".)

EITHER do step 1.1 OR step 1.2

1.1 Dynamically configure

  (pushnew (pathname "/path/to/emotiq/src/") asdf:*central-registry* :test 'equal)

1.2 Configure to have the lisp dynamically scan the filesystem at boot
for ASDF source registry DSL artifacts.  

Assuming the source to <https://github.com/Emotiq/emotiq> has been
cloned to <file:///~/work/github.com-Emotiq/emotiq/>,

  (uiop:run-program "mkdir -p ~/.config/common-lisp/source-registry.conf.d/ && cp ~/work/github.com-Emotiq/emotiq/etc/emotiq.conf ~/.config/common-lisp/source-registry.conf.d/")

2. Ensure Quicklisp dependencies have been satified

  (ql:quickload :emotiq-tests)

3. Evaluate form to test, which also loads, the system:

  (asdf:test-system :emotiq)

4. At end you should see a result like
  
    Unit Test Summary
     | 12 assertions total
     | 12 passed
     | 0 failed
     | 0 execution errors
     | 0 missing tests

The counts of assertions/passed should go up over time, and should
stay equal, with other counts staying zero.

|#
