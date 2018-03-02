;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "cosi-test"
  :depends-on (cosi lisp-unit)
  :perform (test-op (o c) (symbol-call :lisp-unit :run-tests
                                       :all (slot-value s 'asdf/component:name))) 
  :components ((:module tests
                        :pathname "./"
                        :components ((:file "test-package")))))

  
  


