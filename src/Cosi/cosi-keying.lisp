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
  #-:ELLIGATOR
  (let* ((skey  (compute-deterministic-skey seed index))
         (plist (cosi-dsa +keying-msg+ skey)))
    (list
     :skey  skey
     :pkey  (getf plist :pkey)
     :index index
     :r     (getf plist :r)
     :s     (getf plist :s)))
  #+:ELLIGATOR
  (multiple-value-bind (skey tau ix)
      (compute-deterministic-elligator-skey seed index)
    ;; Note that Elligator encoding may have to search for a suitable
    ;; key. Hence the index returned may be different from the index
    ;; suggested.
    (declare (ignore tau))
    (let ((plist (cosi-dsa +keying-msg+ skey)))
      (list
       :skey  skey
       :pkey  (getf plist :pkey)
       :index ix
       :r     (getf plist :r)
       :s     (getf plist :s)
       ))))

(defun make-unique-deterministic-keypair (seed)
  ;; create a unique deterministic keypair, using a Bloom filter to
  ;; record keys, and adding an incrementing index to the seed until
  ;; we have uniqueness.
  (um:nlet-tail iter ((ix  0))
    (let* ((plist (make-deterministic-keypair seed ix))
           (pkey  (getf plist :pkey))
           (proof (list (getf plist :r) (getf plist :s))))
      (if (unique-key-p pkey proof)
          ;; when was unique, it also got added to table
          plist
        (iter (1+ ix))))))

;; --------------------------------------------------------------

(defun get-seed (seed)
  (if seed
      (ed-convert-int-to-lev (need-integer-form seed))
    (ctr-drbg 256)))

(defun get-salt (salt)
  (let ((pref (loenc:encode "salt")))
    (if salt
        (concatenate 'vector
                     pref 
                     (ed-convert-int-to-lev (need-integer-form salt)))
      pref)))

;; --------------------------------------------------------------
;; User level access functions for keying

(defun make-random-keypair (&optional seed salt)
  ;; seed can be anything at all, any Lisp object
  ;; This is the normal entry point when making new user keys.
  (let* ((seed  (get-seed seed))
         (salt  (get-salt salt))
         (rseed (ironclad:pbkdf2-hash-password seed
                                               :salt       salt
                                               :digest     :sha3
                                               :iterations 2048)))
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
;; Note that no space is saved if we settle for p = 0.01 instead of
;; 0.001. The only thing that happens with higher false positive
;; probability is that we need only 7 hashes instead of 10.
;;

(defvar *pkey-filter*
  (bloom-filter:make-bloom-filter :nitems  1000000
                                  :pfalse  0.001
                                  :hash-fn 'identity))
(defvar *pkeys* (maps:empty)) ;; for now...

(defun true-pkey (pkey)
  (#-:ELLIGATOR identity
   #+:ELLIGATOR to-elligator-range
    (need-integer-form pkey)))

(defun unique-key-p (pkey proof)
  (let* ((pk    (true-pkey pkey))
         (hashv (ed-convert-int-to-lev pk)))
    (um:critical-section ;; for SMP safety
      (unless (bloom-filter:test-membership *pkey-filter* hashv)
        (bloom-filter:add-obj-to-bf *pkey-filter* hashv)
        (setf *pkeys* (maps:add pk proof *pkeys*))
        (cosi-blkdef:add-key-to-block pkey proof)
        t)
      )))

(defun lookup-pkey (pkey)
  ;; return t if pkey found and valid, nil if not found or invalid
  (let ((proof (maps:find (true-pkey pkey) *pkeys*)))
    (when proof
      (apply 'validate-pkey pkey proof))))


(defun NYI (&rest args)
  (error "Not yet implemented ~A" args))

(defun #1=populate-pkey-database ()
  ;; this should populate *pkeys* and *pkey-filter* from the blockchain at startup
  (NYI '#1#))

(defun #1=refresh-pkey-database ()
  ;; periodically refresh the database from blockchain updates
  (NYI '#1#))

;; --------------------------------------------------------------------

(defvar *wordlist-folder*  #P"~/Desktop/Emotiq/Research-Code/src/Cosi/wordlists/")
(defvar *ws* '(#\space #\tab #\linefeed #\newline #\page))

(defun import-wordlist (filename)
  ;; import a 2048 word list for use in wordlist encoding/decoding
  ;; E.g., (import-wordlist "english.txt")
  (let ((wvec (coerce
               (um:accum acc
                 (with-open-file (f (merge-pathnames
                                     *wordlist-folder* 
                                     filename)
                                    :direction :input)
                   (um:nlet-tail iter ()
                     (let ((wrd (read-line f nil f)))
                       (unless (eql wrd f)
                         (let ((trimmed (string-trim *ws* wrd)))
                           (when (plusp (length trimmed))
                             (acc trimmed)))
                         (iter))))))
               'vector)))
    (unless (= 2048 (length wvec))
      (error "Invalid master word list"))
    wvec))

(defvar *english*  (import-wordlist "english.txt"))

(defun convert-int-to-wordlist (val &optional (wref *english*))
  ;; convert a positive, or zero, 256-bit integer value to a list of
  ;; words representing little-endian encoding in 11-bit groups. The
  ;; integer has another MSB of 8 bits prepended from SHA3/256 of its
  ;; value, to make it a multiple of 11 bits wide, for 24 words in the
  ;; final wordlist.
  ;;
  ;; wref is a vector of 2048 words chosen from some language wordlist.
  ;;
  (assert (= 2048 (length wref)))
  (check-type val (integer 0))
  (assert (<= (integer-length val) 256))
  (let* ((h  (sha3/256-buffers (ed-convert-int-to-lev val 32)))
         (v  (dpb (aref h 0) (byte 8 256) val)))
    (loop for ct from 0 below 24
          for pos from 0 by 11
          collect (aref wref (ldb (byte 11 pos) v)))
    ))

(defun convert-wordlist-to-int (wlist &optional (wref *english*))
  ;; convert a list of 24 words from a wordlist into an integer with
  ;; each word representing an 11-bit group presented in little-endian
  ;; order. The result is a 264-bit integer, which is a 256-bit
  ;; integer plus a randomized top 8 bits. The MSB byte (8 bits) must
  ;; match the SHA3/256 of the final 256-bit value.
  ;;
  ;; wref is a vector of 2048 words chosen from some language wordlist.
  ;;
  (assert (= 2048 (length wref)))
  (assert (and (consp wlist)
               (every 'stringp wlist)
               (= 24 (length wlist))))
  (let ((v 0))
    (loop for wrd in wlist
          for pos from 0 by 11
          do
          ;; this will error if word isn't found in list...
          (setf (ldb (byte 11 pos) v) (position wrd wref)))
    (let* ((val (ldb (byte 256 0) v))
           (h   (sha3/256-buffers (ed-convert-int-to-lev val 32))))
      (unless (= (aref h 0) (ldb (byte 8 256) v))
        (error "Invalid wordlist"))
      val)))

