;;;; emotiq.asd

(defsystem #:emotiq
  :description "Emotiq"
  :license "MIT (see LICENSE.txt)"
  :depends-on (#:ironclad #:bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "external"))
  :in-order-to ((test-op (test-op "emotiq-tests"))))


(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system '#:emotiq))))
  (asdf:load-system '#:emotiq)
  (funcall (find-symbol "RUN-TESTS" '#:lisp-unit) :all '#:emotiq-tests))



#| Abbreviated instructions for a test build in the Lisp REPL:

0. [Something about assumption that Quicklisp has been set up....!] 

1. Tell ASDF where to find system: (Substitute as appropriate for "/path/to/".)

  (pushnew (pathname "/path/to/emotiq/src/") asdf:*central-registry* :test 'equal)

2. Evaluate form to test, which also loads, the system:

  (asdf:test-system :emotiq)

3. At end you should see a result like
  
    Unit Test Summary
     | 11 assertions total
     | 11 passed
     | 0 failed
     | 0 execution errors
     | 0 missing tests

The counts of assertions/passed should go up over time, and should stay equal,
with other counts staying zero.

|#
