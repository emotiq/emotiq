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

(defpackage :cosi-keying
  (:use
   :common-lisp
   :ecc-crypto-b571
   :crypto-mod-math
   :edwards-ecc)
  (:export
   :make-keypair
   :make-deterministic-keypair
   :validate-pkey
   :ed-dsa
   :ed-dsa-validate
   :need-integer-form
   :published-form
   ))

(in-package :cosi-keying)

;; NOTE: The adopted standard for Emotiq (for now) is:
;;    Hashes are hex strings, Binary data are base64 strings. And
;;    while keying uses hashing internally, published keying info is
;;    binary, not hashes. So key info will be presented as base64
;;    strings.
;;
;;    Users are identified by public key.

;; -----------------------------------------------

(defun mod-r (v)
  (mod v *ed-r*))

(defun hash-to-int (vec)
  (mod-r (ed-convert-lev-to-int vec)))

(defun add-mod-r (a b)
  (add-mod *ed-r* a b))

(defun sub-mod-r (a b)
  (sub-mod *ed-r* a b))

(defun mult-mod-r (a b)
  (mult-mod *ed-r* a b))

;; ---------------------------------------------------

(defmethod need-integer-form ((v integer))
  v)

(defmethod need-integer-form ((v vector)) ;; assumed to be ub8v-le
  (ed-convert-lev-to-int v))

(defmethod need-integer-form ((v string)) ;; assumed to be base64 string
  (need-integer-form (decode-bytes-from-base64 v)))

(defmethod need-integer-form ((v ecc-pt))
  (ed-compress-pt v))

(defmethod need-integer-form ((v ed-proj-pt))
  (need-integer-form (ed-affine v)))

;; -----------------

(defmethod published-form ((v integer))
  (published-form (ed-convert-int-to-lev v)))

(defmethod published-form ((v vector)) ;; assumed to be ub8v le
  (encode-bytes-to-base64 v))

(defmethod published-form ((v ecc-pt))
  (published-form (ed-compress-pt v)))

(defmethod published-form ((v ed-proj-pt))
  (published-form (ed-compress-pt v)))

(defmethod published-form ((v string)) ;; assumed to be base64
  (if (ignore-errors
        (decode-bytes-from-base64 v))
      v
    (call-next-method)))

(defmethod published-form (v)
  (encode-bytes-to-base64 (loenc:encode v)))

;; ---------------------------------------------------
;; The IRTF EdDSA standard as a primitive

(defun ed-dsa (msg skey)
  (let* ((skey  (need-integer-form skey))
         (h     (ed-convert-lev-to-int
                 (sha3-buffers
                  ;; full 512 bits
                  (ed-convert-int-to-lev skey))))
         (a     0)
         ;; the constant 3 is for cofactors of 8 or lower pow2
         (bits  (byte (- *ed-nbits* 5) 3)))
    (setf (ldb bits a) (ldb bits h)
          (ldb (byte 1 (1- *ed-nbits*)) a) 1)
    (let* ((msg-enc   (loenc:encode msg))
           (pkey      (ed-nth-pt a))
           (pkey-cmpr (ed-compress-pt pkey))
           (r         (ed-convert-lev-to-int
                       (sha3-buffers
                        (ed-convert-int-to-lev (ldb (byte *ed-nbits* *ed-nbits*) h))
                        msg-enc)))
           (rpt       (ed-nth-pt r))
           (rpt-cmpr  (ed-compress-pt rpt))
           (s         (add-mod-r r
                                 (mult-mod-r a
                                             (ed-convert-lev-to-int
                                              (sha3-buffers
                                               (ed-convert-int-to-lev rpt-cmpr)
                                               (ed-convert-int-to-lev pkey-cmpr)
                                               msg-enc))
                                             ))))
      (list
       :msg   msg
       :pkey  (published-form pkey-cmpr)
       :r     (published-form rpt-cmpr)
       :s     (published-form s))
      )))

(defun ed-dsa-validate (msg pkey r s)
  (let ((pkey (need-integer-form pkey))
        (r    (need-integer-form r))
        (s    (need-integer-form s)))
    (ed-pt=
     (ed-nth-pt s)
     (ed-add (ed-decompress-pt r)
             (ed-mul (ed-decompress-pt pkey)
                     (ed-convert-lev-to-int
                      (sha3-buffers
                       (ed-convert-int-to-lev r)
                       (ed-convert-int-to-lev pkey)
                       (loenc:encode msg)))
                     )))))

;; --------------------------------------------

(defconstant +keying-msg+  #(:keying-{61031482-17DB-11E8-8786-985AEBDA9C2A}))

(defun make-deterministic-keypair (seed)
  ;; WARNING!! This version is for testing only. Two different users
  ;; who type in the same seed will end up with the same keying. We
  ;; can't allow that in the released system.
  (let* ((skey  (ldb (byte (1+ *ed-nbits*) 0)
                     (ed-convert-lev-to-int
                      (sha3-buffers (loenc:encode seed)))))
         (plist (ed-dsa +keying-msg+ skey))
         (pkey  (getf plist :pkey))
         (s     (getf plist :s))
         (r     (getf plist :r)))
    (list
     :skey skey
     :pkey (published-form pkey)
     :r    (published-form r)
     :s    (published-form s))))

(defun top-octave-rand (range)
  ;; random value in the top octave of the range
  (random-between (ash range -1) range))

(defun make-keypair (seed)
  ;; seed can be anything at all, any Lisp object
  (make-deterministic-keypair (list seed
                                    (top-octave-rand *ed-r*))))

(defun validate-pkey (pkey r s)
  (ed-dsa-validate +keying-msg+ pkey r s))
        
;; ------------------------------------------------------

