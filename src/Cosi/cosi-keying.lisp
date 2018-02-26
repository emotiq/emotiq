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
;; Keying for attribution. User keys, and signatures. You really don't
;; need such an elaborate system when generating keys for internal
;; math operations like for range proofs and such. And range proofs
;; also present special problems with respect to Elligator encodings.

(defconstant +keying-msg+  #(:keying-{61031482-17DB-11E8-8786-985AEBDA9C2A}))

(defun make-deterministic-keypair (seed &optional (index 0))
  ;; WARNING!! This version is for testing only. Two different users
  ;; who type in the same seed will end up with the same keying. We
  ;; can't allow that in the released system.
  (let* ((skey  #-:ELLIGATOR (compute-deterministic-skey seed index)
                #+:ELLIGATOR (compute-deterministic-elligator-skey seed index))
         (plist (cosi-dsa +keying-msg+ skey)))
    (list
     :skey  skey
     :pkey  (getf plist :pkey)
     :index index
     :r     (getf plist :r)
     :s     (getf plist :s)
     )))

(defun make-unique-deterministic-keypair (seed)
  ;; create a unique deterministic keypair, using a Bloom filter to
  ;; record keys, and adding an incrementing index to the seed until
  ;; we have uniqueness.
  (um:nlet-tail iter ((ix  0))
    (let* ((plist (make-deterministic-keypair seed ix))
           (pkey  (getf plist :pkey))
           (proof (list (getf plist :r) (getf plist :s))))
      (if (unique-key-p pkey proof) ;; this also adds the key to the table if missing
          plist
        (iter (1+ ix))))
    ))

(defun make-random-keypair (&optional seed)
  ;; seed can be anything at all, any Lisp object
  (let ((rseed (list seed (ctr-drbg 256))))
    (make-unique-deterministic-keypair rseed)))

(defun make-subkey (skey sub-seed)
  ;; Need to do one at a time, since unique may have created an index
  ;; value, and you can't retrace the path without that index value.
  ;; That is to say, you really do need to keep a record of all the
  ;; private keys along the way...
  (make-unique-deterministic-keypair
   (list (need-integer-form skey)
         sub-seed)))

(defun validate-pkey (pkey r s)
  (cosi-dsa-validate +keying-msg+ pkey r s))
        
;; ------------------------------------------------------

;; Bloom filter sizing.... Suppose we want false positives rate
;; P_false < 0.001, number of hashes needed is K = -log2 p = 9.97 =
;; 10, and bits/item M/N = -1.44 Log2 p = 14.35. Suppose further, that
;; we plan for N = 1M items in the filter.
;;
;; We can chop up the bits of a hash like SHA256 for use as the
;; separate hash values, easier to do if we just use whole octets from
;; the hash directly. And keeping M as a power of 2, enables just
;; masking the hash value with 2^m-1 for indexing into the bit table..
;;
;; For 1M items, we need M = 14.35M bits => 16M or 24 bit addressing.
;; With 10 hashes, we need 240 bits of hash overall. And the bit table
;; will occupy 2 MB of memory.
;;
;; Pkeys are all at least 249 bits, so we are okay with just using the
;; Pkey as the source of hash values for the Bloom filter. We just
;; need to convert them into a little-endian vector of bytes.
;;
;; Not that no space is saved if we settle for p = 0.01 instead of
;; 0.001. The only thing that happens with higher false positive
;; probability is that we need only 7 hashes instead of 10.
;;

(defvar *pkey-filter*
  (bloom-filter:make-bloom-filter :nitems 1000000
                                  :pfalse 0.001
                                  :hashfn 'identity))
(defvar *pkeys* (maps:empty)) ;; for now...

(defun unique-key-p (pkey proof)
  (let ((hashv (ed-convert-int-to-lev (need-integer-form pkey))))
    (um:critical-section ;; for SMP safety
      (unless (bloom-filter:test-membership *pkey-filter* hashv)
        (bloom-filter:add-obj-to-bf *pkey-filter* hashv)
        (setf *pkeys* (maps:add pkey proof *pkeys*))
        ;; maybe also add pkey+proof to blockchain?
        ;; (add-key-to-blockchain pkey proof)
        t))))

(defun lookup-pkey (pkey)
  ;; return t if pkey found and valid, nil if not found or invalid
  (let ((proof (maps:find pkey *pkeys*)))
    (when proof
      (apply 'validate-pkey pkey proof))))


(defun NYI (&rest args)
  (error "Not yet implemented ~A" args))

(defun #1=add-key-to-blockchain (pkey proof)
  (declare (ignore pkey proof)
  (NYI '#1#))

(defun #1=populate-pkey-database ()
  ;; this should populate *pkeys* and *pkey-filter* from the blockchain at startup
  (NYI '#1#))
