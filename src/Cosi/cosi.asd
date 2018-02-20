;; ads.asd -- ASD File for Authenticated Data Structures
;;
;; DM/Emotiq  01/18
;; -----------------------------------------------------------
#|
The MIT License

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


(in-package :cl-user)

#+:ALLEGRO (require :sock)

(asdf:defsystem "cosi"
  :description "Cosi: Authenticated multi-signatures in Lisp"
  :version     "1.0"
  :author      "D.McClain <dbm@emotiq.ch>"
  :license     "Copyright (c) 2018 by Emotiq, A.G. License terms apply."
  :components  (
		(:file "cosi-async")

                ;; (:file "cosi")
		;; (:file "tst-cas")
                ;; (:file "cosi-socket")

                (:file "cosi-construction")
                (:file "cosi-sockets")
                (:file "cosi-handlers")

                (:file "range-proofs")
                (:file "confidential-purchase")
                
                ;; (:file :random-partition)
                )
  :SERIAL T
  :depends-on   (:ironclad
		 :actors
                 :core-crypto
                 :lisp-object-encoder
                 :useful-macros
                 :usocket
                 ))

