;; basic-boxing.lisp -- a collection of boxed basic types to help us along
;;
;; DM/Emotiq  01/18
;; -----------------------------------------------------------------------------------
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


(in-package :ads)

(defclass auth-component ()
  ;; abstract base class for special types needed in the development
  ;; of authenticated types - Witness, Hash-val, Digest, Prover
  ())

;; -----------------------------------
;; Boxed types for training wheels...
;;
;; The premise here is that a witness value must be a flat, hashable,
;; representation of a data structure and the values that they are
;; witnessing. Witness values are provided across the network to
;; remote verifier nodes. Hash values are also provided, traditionally
;; in hex-string form. And from provided witness values, the remote
;; verifier nodes must be able to compute the same hash values as
;; provided by the prover nodes.
;;
;; Internally, our Lisp produces ub8 vectors for both witness values
;; and hash values. Hash values can only be computed for flat ub8
;; vector values. And our witness vectors can be decoded to
;; unambiguously reconstruct a deep copy of the original data and its
;; structure.
;;
;; However, remote nodes might not be running a Lisp image, and thus,
;; be unable to reconstruct complex Lisp data structures from witness
;; value vectors. Or else, they may have limited abilities to
;; reconstruct data, e.g., C-based code. This is likely why Etherium
;; uses such a constricted set of data types.
;;
;; The ADS paper describes a flattening operation, called SHALLOW,
;; that converts elaborate data structures into flat tuples of data.
;; But those data can still individually be compound types, such as
;; strings mixed with integers. How big is an integer, what is its
;; range, etc.? What about strings, like hash value hex-strings? How
;; are strings represented? A string is a vector, so a vector of
;; strings mixed with integers would not be a simple flat structure at
;; all.
;;
;; Let's assume that strings will have a common representation, and
;; will be understood by all nodes. Let's also assume that ub8 vectors
;; are understood, probably as a basis for understanding strings. But
;; let's not assume that vectors of mixed data types, such as strings
;; mixed with integers (or worse), will be understood. (I would argue
;; that at least a few more primitive data types also be understood,
;; such as IEEE floating point values, tuples of mixed types,
;; homogeneous lists and vectors. But for transmission, these would
;; all be reduced to ub8 vectors.)
;;
;; So it would appear that ub8 vectors are the most basic data
;; structure for interchange. And the ADS shallow tuples will need to
;; be further recast as flat ub8 vectors for interchange. We also need
;; to force SHALLOW tuples to ub8 vector form for purposes of hashing.
;; And our internally hashed ub8 vectors must be the same as used by
;; verifier nodes so that they can obtain the same hash values for
;; proof matching.
;;
;; But let's push that interchange conversion out to the node
;; interfacing layer, and not be constrained here. For debugging
;; purposes a human readable print representation may be desirable,
;; but that should not necessarily be the stored internal form.
;;
;; -------------------------------
;;
;; Now what about hash values? The tradition has been to present hash
;; values as hex-strings. But hashing can't be performed on
;; hex-strings until they are converted to equivalent ub8 vector form.
;;
;; There are two ways to produce a ub8 vector from a hex string.
;; Either we perform a semantically aware conversion back to vector
;; form, nibble by nibble, or else we use a generalized string to
;; vector encoding.  The two approaches will produce two different ub8
;; vectors, and they will hash to different values.
;;
;; Since you can't usefully perform any operation on a hash value
;; except comparison with other hash values, there is, perhaps, no
;; reason to prefer anything other than hex-string form for all hash
;; values.
;;
;; (Not entirely true - think about a SHA3 hash value as providing
;; multiple hashes needed for generation of Bloom filters...)
;;
;; But since witness values will be converted to / from hex-string
;; form for interchange, and they *do* need to be converted with
;; semantic awareness, nibble by nibble, that may as well be true for
;; our hash hex-strings too. I.e., we should use semantic conversion
;; of hex strings back to the ub8v form, nibble-by-nibble conversion.
;;
;; -------------------------------------------------------------------

(defclass hex-string ()
  ;; Hex-strings -- semantic conversion back to ub8 vectors
  ;; boxing for type discrimination
  ((str  :reader   hex-string-str
         :initarg  :str
         :initform ""
         :type     string)))

(defmethod make-hex-string ((hs hex-string))
  hs)

(defmethod make-hex-string (obj)
  (make-instance 'hex-string
                 :str (to-string obj)))

(defmethod make-hex-string ((v vector))
  (cond ((typep v 'vector-ub8)
         (make-instance 'hex-string
                        :str (encode-bytes-to-string v)))

        (t
         (make-hex-string (to-ub8v v)))
        ))

;; ----------------------------------------------------------
;; Conversion to primitive Lisp string form...

(defmethod to-string ((s string))
  ;; to basic Lisp string type
  s)

(defmethod to-string ((hs hex-string))
  (hex-string-str hs))

(defmethod to-string (x)
  (to-string (make-hex-string (to-ub8v x))))

;; ------------------------------------------------------------
;; Conversion to primitive (i.e., unboxed) ub8 vectors

(defmethod to-ub8v (x)
  (loenc:encode x))

(defmethod to-ub8v ((hs hex-string))
  (decode-string-to-bytes (hex-string-str hs)))

(defmethod to-ub8v ((v vector))
  (cond ((typep v 'vector-ub8)  v)
        (t  (call-next-method))))

;; ----------------------------------------------
;; A boxed representation of a WITNESS
;;
;; WHat is a WITNESS? The world is split into PROVERS and VERIFIERS.
;; PROVERS own the actual authenticated data structure. This structure
;; is constructed so that actions taken by the PROVER can be proven by
;; any VERIFIER who is simply given a result, a list of WITNESS
;; values, and the hash digest for the top of the data structure.
;;
;; So on the PROVER side of the world, a WITNESS is a shallow
;; projection of a portion of the underlying data structure, e.g., a
;; tree node or leaf, with all recursive references replaced by hash
;; digests. To get the hash digest of the witness snippet, we
;; serialize to a network-portable vector of octets that can be used
;; by anyone to fully reconstruct that snippet of data structure.
;; Hashing can only be applied to simple octet vectors, such as this
;; network-portable format.
;;
;; [NOTE: Even though the serialized witness vector contains
;; sufficient information to fully reconstruct a portion of the data
;; structure with hash values in place of memory pointers, a verifier
;; never actually does this. It merely computes the hash of the
;; provided witness vector and folds that into a running hash
;; accumulator. But the important thing to note about the serialized
;; witness value is that it captures not only the hash values and
;; basic data values, but it also captures its structural form - i.e.,
;; whether it was a list, or a vector, or a class instance object.
;; That surrouding type context is important for distinguishing the
;; nature of provided witness values.]
;;
;; On the VERIFIER side, a WITNESS is the received octet vector,
;; already in a form suitable for hashing. The VERIFIER also gets a
;; hash of the top of the data structure, and any parameters that
;; guided the operation of the PROVER.
;;
;; For example, a FETCH operation needs a path (a list of left/right
;; turns along a branch of a binary tree), in order to reach a leaf
;; element.
;;
;; That path guides the actions of the PROVER during the FETCH
;; operation, and that same path is provided to all VERIFIERs.
;; WITNESS values are accumulated by the PROVER at each junction of
;; the branch traversal. That list of WITNESS values is also provided
;; to each VERIFIER, as well as a single hash value that represents
;; the top of the data structure.
;;
;; By following the same data struture source code on the VERIFIER
;; side, armed with only the top level hash, the witness list, and the
;; parameters used for the proven operation (FETCH, UPDATE, DISCARD),
;; the verifier can fully evaluate the proof and wind up with the same
;; answer, all cryptographically proven correct. No trust need be
;; assumed among any of the participants. The proof is indisputable
;; and cannot be forged nor denied. And the VERIFIER does not need to
;; duplicate the full data structure in order to achieve this. It only
;; needs hash values along the path, furnished by the witness values
;; and the accumlating hash of the VERIFIER's own calculations.
;;
;; How can the same source code be used to do both PROVER and
;; VERIFIER? It's all in the magic of CLOS and dynamic method multiple
;; dispatch. Thanks to Lisp, this (relatively) simple system can be
;; used to construct fully authenticated data structues of any type.
;; The user needs only to code a few simple methods for a new data
;; structure, and the core of this system takes it from there.

(defclass witness (auth-component)
  ;; always a component of a data structure containing only Digests or
  ;; raw data
  ((val  :reader  witness-val :initarg :val)))

(defmethod witness-p (x)
  nil)

(defmethod witness-p ((wval witness))
  t)

(defmethod to-ub8v ((wv witness))
  (to-ub8v (witness-val wv)))

(defmethod print-object ((wv witness) stream)
  (format stream "#<Witness: ~A>" (to-string wv)))

;; ---------------------------------------------
;; A boxed representation of a computed hash value (ub8v form)

(defclass hash-val (auth-component)
  ;; always a ub8v
  ((ubvec  :reader  hash-val-ubvec  :initarg :ubvec)))

(defmethod to-ub8v ((hv hash-val))
  (hash-val-ubvec hv))

(defmethod print-object ((hv hash-val) stream)
  (format stream "#<Hash-val: ~A>" (to-string hv)))

;; ----------------------------------------------------------------

(defgeneric hash (val)
  (:method (val)
   ;; (does it break anything by having a catchall?)
   (make-instance 'hash-val
                  :ubvec (basic-hash/256 val)))
  (:method ((val witness))
   (hash (witness-val val))))

(defmethod hash= ((h1 hash-val) (h2 hash-val))
  (string-equal (to-string h1) (to-string h2)))

(defmethod compare ((k1 hash-val) (k2 hash-val))
  (compare (hash-val-ubvec k1) (hash-val-ubvec k2)))

