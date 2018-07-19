#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

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

(asdf:defsystem "core-crypto"
  :description "core-crypto: core cryptography functions"
  :version     "1.0"
  :author      "D.McClain <dbm@refined-audiometrics.com>"
  :in-order-to ((test-op (test-op "core-crypto-tests")))
  :license     "Copyright (c) 2015 by Refined Audiometrics Laboratory, LLC. All rights reserved."
  :serial       t
  :components  ((:file "ecc-package")
                (:file "cached-var")
                (:file "modular-arith")
                (:file "utilities")
                (:file "vec-repr")
                (:file "hash")
                (:file "ctr-hash-drbg")
                (:file "primes")
                #+(AND :MACOSX :COM.RAL) (:file "crypto-le")
                (:file "kdf")
                (:file "gf-571")
                (:file "lib-loads")
                (:file "edwards")
                (:file "ecc-B571")
                (:file "curve-gen")
                (:file "crypto-environ")
                #+(AND :MACOSX :COM.RAL) (:file "machine-id")
                (:file "lagrange-4-square"))
  :depends-on   ("ironclad"
                 #+:COM.RAL "aesx"
                 "useful-macros"
                 "mpcompat"
                 "lisp-object-encoder"
                 "s-base64"
                 "emotiq"
                 "emotiq/delivery"
                 "cffi"
		 "core-crypto/libraries"
                 "crypto-pairings/libraries"
                 ))

(defsystem "core-crypto/libraries"
  :perform
  (prepare-op
   :before (o c)
   (let ((wildcard-for-libraries
          (make-pathname :defaults 
                         (asdf:system-relative-pathname
                          :emotiq "../var/local/lib/libLispCurve1174")
                         :type :wild)))
     (unless (directory wildcard-for-libraries)
       (format *standard-output*
               "~&Failed to find libraries matching~&~t~a~&~
~&Attempting to build native libraries... hang on for a minute, please..."
               wildcard-for-libraries)
       (run-program `("bash"
                      ,(namestring (system-relative-pathname
                                    :emotiq "../etc/build-crypto-ecc.bash")))
                    :output :string :error :string)
       (format *standard-output* "~tWhew!  Finished.~&")))))

(defsystem "crypto-pairings/libraries"
  :perform
  (prepare-op
   :before (o c)
   (let ((wildcard-for-libraries
          (make-pathname :defaults 
                         (asdf:system-relative-pathname
                          :emotiq "../var/local/lib/libLispPBCIntf")
                         :type :wild)))
     (unless (directory wildcard-for-libraries)
       (format *standard-output*
               "~&Failed to find libraries matching~&~t~a~&~
~&Attempting to build native libraries... hang on for a minute, please..."
               wildcard-for-libraries)
       (run-program `("bash"
                      ,(namestring (system-relative-pathname
                                    :emotiq "../etc/build-crypto-pairings.bash")))
                    :output :string :error :string)
       (format *standard-output* "~tWhew!  Finished.~&")))))

