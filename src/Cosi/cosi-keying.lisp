;; cosi-keying.lisp -- Keying for Cosi networks
;;
;; DM/Emotiq 02/18
;; ----------------------------------------------------------------
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

(in-package :cosi-keying)

;; NOTE: The adopted standard for Emotiq (for now) is:
;;    Hashes are hex strings, Binary data are base58 strings. And
;;    while keying uses hashing internally, published keying info is
;;    binary, not hashes. So key info will be presented as base64
;;    strings.
;;
;;    Users are identified by public key.

;; ---------------------------------------------------

(defmethod need-integer-form ((v integer))
  v)

(defmethod need-integer-form ((v vector)) ;; assumed to be ub8v-le
  (ed-convert-lev-to-int v))

(defmethod need-integer-form ((v string)) ;; assumed to be base64 string
  (need-integer-form (base58:decode v)))

(defmethod need-integer-form ((v ecc-pt))
  (ed-compress-pt v))

(defmethod need-integer-form ((v ed-proj-pt))
  (need-integer-form (ed-affine v)))

(defmethod need-integer-form (v)
  (need-integer-form (loenc:encode v)))

;; -----------------

(defmethod published-form ((v integer))
  (base58:encode v))

(defmethod published-form ((v vector)) ;; assumed to be ub8v le
  (published-form (need-integer-form v)))

(defmethod published-form ((v ecc-pt))
  (published-form (need-integer-form v)))

(defmethod published-form ((v ed-proj-pt))
  (published-form (need-integer-form v)))

(defmethod published-form ((v string)) ;; assumed to be base64
  (if (ignore-errors
        (base58:decode v))
      v
    (call-next-method)))

(defmethod published-form (v)
  (published-form (need-integer-form v)))

;; ---------------------------------------------------
;; The IETF EdDSA standard as a primitive

(defun cosi-dsa (msg skey)
  #-:ELLIGATOR
  (let ((quad  (ed-dsa msg skey)))
    (list
     :msg  (getf quad :msg)
     :pkey (published-form (getf quad :pkey))
     :r    (published-form (getf quad :r))
     :s    (published-form (getf quad :s))
     ))
  #+:ELLIGATOR
  (let ((quad (elligator-ed-dsa msg skey)))
    (list
     :msg  (getf quad :msg)
     :pkey (published-form (getf quad :tau-pub))
     :r    (published-form (getf quad :tau-r))
     :s    (published-form (getf quad :s))
     )))

(defun cosi-dsa-validate (msg pkey r s)
  (let ((pkey (need-integer-form pkey))
        (r    (need-integer-form r))
        (s    (need-integer-form s)))
    (#-:ELLIGATOR ed-dsa-validate 
     #+:ELLIGATOR elligator-ed-dsa-validate
     msg pkey r s)))

;; --------------------------------------------

(defconstant +keying-msg+  #(:keying-{61031482-17DB-11E8-8786-985AEBDA9C2A}))

(defun make-deterministic-keypair (seed)
  ;; WARNING!! This version is for testing only. Two different users
  ;; who type in the same seed will end up with the same keying. We
  ;; can't allow that in the released system.
  (let* ((skey  #-:ELLIGATOR (compute-skey seed)
                #+:ELLIGATOR (compute-elligator-skey seed))
         (plist (cosi-dsa +keying-msg+ skey)))
    (list
     :skey skey
     :pkey (getf plist :pkey)
     :r    (getf plist :r)
     :s    (getf plist :s)
     )))

(defun make-random-keypair (seed)
  ;; seed can be anything at all, any Lisp object
  (make-deterministic-keypair (list seed
                                    (ctr-drbg 256))))

(defun make-subkey (skey &rest sub-seeds)
  (reduce (lambda (quad sub-seed)
            (make-deterministic-keypair
             (list (need-integer-form (getf quad :skey))
                   sub-seed)))
          sub-seeds
          :initial-value (list :skey skey)))

(defun validate-pkey (pkey r s)
  (cosi-dsa-validate +keying-msg+ pkey r s))
        
;; ------------------------------------------------------

