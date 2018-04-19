;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "emotiq-wallet-test"
  :defsystem-depends-on (prove-asdf)
  :depends-on (prove
               emotiq/wallet)
  :perform (test-op (o s)
              (symbol-call :prove :run s))
  :components ((:module tests :pathname "./"
                :components ((:test-file "wallet")))))



  

