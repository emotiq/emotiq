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

(defsystem "cosi-bls"
  :description "Cosi: Authenticated multi-signatures in Lisp"
  :version     "1.0.3"
  :author      "D.McClain <dbm@emotiq.ch>"
  :license     "Copyright (c) 2018 by Emotiq, A.G. MIT License."
  :depends-on (ironclad
               actors
               core-crypto
               crypto-pairings
               lisp-object-encoder
               useful-macros
               usocket
               trivial-garbage
               ads-clos
               gossip)
  :in-order-to ((test-op (test-op "cosi-bls-tests")))
  :components ((:module package
                        :pathname "./"
                        :components ((:file "package")))
               (:module source
                        :depends-on (package)
                        :pathname "./"
                        :serial t
                        :components (#+CLOZURE (:file "clozure")
                                     (:file "address")
                                     (:file "cosi-blkdef")
                                     (:file "block")
                                     (:file "cosi-keying")
				     (:file "cosi-construction")
                                     (:file "range-proofs")
                                     (:file "transaction")
                                     (:file "cosi-handlers")
                                     (:file "cosi-netw-xlat")))))




                        
