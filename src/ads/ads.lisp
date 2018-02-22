;; ads.lisp -- provably secure data structures
;;
;; DM/Emotiq 01/18
;; --------------------------------------------------------------------
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


(in-package :ADS)

;; -----------------------------------------------------------------------------
#|
 The big idea here, borrowed from the paper:

 "Authenticated Data Structures, Generically"
    by Miller, Hicks, Katz, and Shi

is to build up base data types that can be used to assist the
construction of arbitrary data types that can also have cryptographics
proofs automatically generated and verified. Herewith, the paper is
denoted ADS.

Initially, we work with training wheels on, implementing many methods
to catch improper use of ADS primitives. Once we have debugged the
data structures we can go back and take off the training wheels.

As a data structure is constructed / modified, a Prover carries along
a value (an ordinary data type) and the hash of its shallow deep copy.
A shallow deep copy is a vector of unsigned-byte-8 that can be used to
reconstruct the original object, yet this vector is amenable to hash
computation.

During construction / modification of the data structure, a proof log
is accumulated which consists of a list of these shallow deep copy
vectors, called proof witnesses. For now we box the witness vectors
into a class object for type checking. This is one of the training
wheels.

And for now... this code is written for single-thread semantics.
Later, we can add locks, or place it under the custody of an Actor.

These basic Authenticated types (Witness, Hash-val, Prover,
Digest) encapsulate the essence of cryptographic proofs. A mode switch
will determine which of proof generation or proof verification will be
used at runtime.

|#

;; -----------------------------------------------------------------------------

(defclass proof-log (queue)
  ;; Type Proof-log = Witness List
  ())

(defun make-proof-log (&optional wlst)
  ;; make a proof log from the provided list of proofs
  ;; A proof list is an ordered list of witness values
  (assert (every 'witness-p wlst))
  (make-instance 'proof-log
                 :initial-contents wlst))
                 
(defvar *current-proof* (make-proof-log))

;; ---------------------------------------------

(defmethod extend-proof ((log proof-log) (new witness))
  ;; add one more proof to the list, at the tail end
  (queue-add log new))

(defmethod next-proof ((log proof-log))
  ;; next proof in the list, popping list
  (queue-pop log))

(defmethod get-proof-list ((log proof-log))
  ;; return the list of proofs in the log, emptying the log
  (queue-contents log))

;; -------------------------------------------------------------------------------

(defclass auth-type (auth-component)
  ;; abstract base class for authenticated data types that participate
  ;; directly in proofs - Digest & Prover
  ;;  Type 'a Auth = Digest (Hash) | Prover ('a * Hash)
  ())

(defclass digest (auth-type)
  ((hashval :reader digest-hashval :initarg :hval)))

(defclass prover (auth-type)
  ((val    :reader  prover-val    :initarg :val)
   (digest :reader  prover-digest :initarg :digest)))

;; ----------------------------------------------------------------------------------
;; ADS Primitives - SHALLOW, AUTH, UNAUTH, HASH
;;
;; Programmers implementing an authenticated data structure will call
;; upon AUTH and UNAUTH as needed.
;;
;; They should also implement specialized SHALLOW methods for their
;; data structure. The job of SHALLOW is to recreate an instance of a
;; data structure element which does not contain any authenticated
;; data types, replacing authenticated components with their hash
;; value.
;;
;; SHALLOW will be called during proof or verification. SHALLOW does
;; not need to produce ub8 vectors. That final conversion is applied
;; internally, as needed, for computing witness and hash values.

(defmethod shallow (x)
  ;; SHALLOW converts structures containing authenticated data types
  ;; to equivalent structures containing only digests in place of
  ;; authenticated elements.
  ;;
  ;; User ADS types should provide specialized versions of SHALLOW.
  ;;
  ;; The identity operation here is suitable for all types *not*
  ;; containing authenticated data types.
  ;;
  x)

(defmethod shallow ((p prover))
  ;; The shallow projection of a Prover is a Digest
  ;;   'a Auth -> 'a Auth
  ;;
  ;; This is the only exception to the rule that SHALLOW should
  ;; convert an object with Authenticated data into the same kind of
  ;; object without authenticated data.
  ;;
  (make-instance 'Digest
                 :hval  (prover-digest p)))

(defmethod shallow ((aval auth-component))
  ;; catch any stray unintentional uses
  (invalid-arg aval))

;; -------------------------
;; AUTH - used to form an Authenticated version of a data structure

(defvar *mode*  :prover) ;; valid modes are :PROVER, :VERIFIER

(defmethod auth (val)
  (ecase *mode*
    (:prover
     ;; convert ordinary val type to a Prover of that val
     (make-instance 'Prover
                    :val    val
                    :digest (hash (shallow val))))
    (:verifier
     ;; convert ordinary val type to a Digest of that val
     (make-instance 'Digest
                    :hval  (hash val)))
    ))
    
(defmethod auth ((x auth-type))
  ;; one of Prover or Digest - already authenticated
  x)

(defmethod auth ((x auth-component))
  ;; catch any stray unintentional uses
  (invalid-arg x))

;; -------------------------
;; UNAUTH - used to convert an Authenticated data structure into a
;; normal (unauthenticated) data structure. It accumulates / checks
;; crypto proofs along the way.

(defmethod unauth ((ads prover))
  ;; convert Prover to ordinary val type, accumulating a witness into
  ;; proof
  (let ((val (prover-val ads)))
    (extend-proof *current-proof*
                  (make-instance 'witness
                                 :val (shallow val)))
    val))

(defmethod unauth ((dig digest))
  ;; convert a Digest into a proven Witness value
  (let ((w  (next-proof *current-proof*)))
    (assert (hash= (digest-hashval dig)
                   (hash w)))
    (witness-val w)))

;; -----------------------------------------------------------------
;; FETCH - like AREF for ADS types. Follow a path (e.g., list of
;; indices) through the data structure to find an element.
;;
;; (privately wondering how we find the path to an element?)

(defmethod fetch ((ads auth-type) path)
  ;; 'a Auth * 'b -> 'a
  ;;
  ;; If ads is a Prover, this accumulates a proof in the proof log. If
  ;; ads is a Digest, this verifies the proofs in the proof log.
  ;;
  (fetch (unauth ads) path))

(defmethod fetch (obj (path null))
  ;; default base case: (FETCH 15 NIL) => 15
  obj)

(defmethod fetch (obj (path cons))
  ;; default base case
  (error "Empty path expected"))

;; -----------------------------------------------------------------
;; UPDATE - purely functional structure update. Path is as mentioned
;; above for FETCH.

(defmethod update ((ads auth-type) path new-val)
  (update (unauth ads) path new-val))

;; -----------------------------------------------------------------
;; DISCARD - purely functional structure update. Path is as mentioned
;; above for FETCH.

(defmethod discard ((ads auth-type) path)
  (discard (unauth ads) path))

;; --------------------------------------------------------------------
;; PROVE -- perform a body of clauses on an authenticated data structure,
;; returning the result of those clauses and the list of witnesses as proof.

(defun prove-fn (fn)
  ;; perform fn as a prover, returning answer and witness list
  (let* ((*current-proof*  (make-proof-log))
         (*mode*           :prover)
         (ans              (multiple-value-list (funcall fn))))
    (values-list (nconc ans (list (get-proof-list *current-proof*))))
    ))

(defmacro prove (&body body)
  `(prove-fn (lambda () ,@body)))

;; ------------------------------------------------
;; VERIFY -- perform proof verification against a list of witness
;; values and a body of clauses operating on a shallow authenticated
;; data structure, returning the result of the clauses.
;;
;; Of possible interest... You can call on the code to construct an
;; authenticated ADS while in VERIFY mode, using a null witness list,
;; to obtain the hash of the entire structure, all while consuming
;; only minimal constant space. This returns the same hash value as
;; found in the top node PROVER after constructing the full data
;; structure in default :PROVER mode.
;;

(defun verify-fn (wlst fn)
  ;; perform fn as a verifier, returning proven answer
  (let* ((*current-proof*  (make-proof-log wlst))
         (*mode*           :verifier)
         (ans              (multiple-value-list (funcall fn))))
    (assert (null (get-proof-list *current-proof*)))
    (values-list ans)))

(defmacro verify (wlist &body body)
  `(verify-fn ,wlist (lambda () ,@body)))

