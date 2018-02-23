;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;; cosi.asd -- ASDF File for Cosigning

#|
Copyright (c) 2018 Emotiq AG

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(defsystem "cosi"
  :description "Cosi: Authenticated multi-signatures in Lisp"
  :version     "1.0"
  :author      "D.McClain <dbm@emotiq.ch>"
  :license     "Copyright (c) 2018 by Emotiq, A.G. MIT License."
  :depends-on (ironclad
               actors
               core-crypto
               lisp-object-encoder
               useful-macros
               usocket)
  :in-order-to ((test-op (test-op "cosi/t")))
  :components ((:module source
                        :pathname "./"
                        :serial t
                        :components ((:file "cosi-async")
                                     (:file "cosi")
                                     (:file "cosi-sockets")
                                     
                                     (:file "cosi-construction")
                                     (:file "cosi-handlers")
                                     
                                     (:file "confidential-purchase")
                                     
                                     (:file "cosi-keying")

                                     (:file "random-partition")
                                     (:file "range-proofs")))))


(defsystem "cosi/test/allegro"
  :description "Allegro timing code from dbm."
  :depends-on (cosi)
  :components ((:module source
                        :pathname "./"
                        :components ((:file "test-cas")))))
                        



(defsystem "cosi/t"
  :defsystem-depends-on (prove-asdf)
  :depends-on (prove cosi)
  :perform (test-op (o c)
              (uiop:symbol-call :prove-asdf :run-test-system c))
  :components ((:module range
                        :pathname "t/"
                        :components ((:test-file "ranges-timing")))))

