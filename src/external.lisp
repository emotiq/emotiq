;;;; external.lisp

(in-package #:emotiq)


;;;; External Software Interfacing

;;; Our interface to external software is for now mostly squeezed into this one
;;; Lisp module. We have for now a few well-chosen external software package,
;;; and use only a small subset of those. For example, we use Ironclad, but only
;;; a rather small subset of it, with an API defined below and abstracted for
;;; our needs.  We ultimately may substitute in some other external
;;; package. When we do, there should be no need to find caller above this
;;; module.  In general, this is how it should go for most external software
;;; systems. However, we may make certain exceptions.  (See below.)

;;; When external packages are used, do NOT use any internal symbol names unless
;;; there is a VERY important reason to do so, and if so, put the reason below,
;;; and say what you're doing about it, such as pestering the developer to
;;; export your symbol, etc.

;;; Let's keep the list of external packages here. Provide a date, description,
;;; motivation, and some URL for additional info.

;;; 19 Jan 2018: Bordeaux Threads: good code, portable (including ACL). Small
;;; API well-known by many developers, unlikely to need replacing. Needed to
;;; support Actors abstraction in support of multithreading. API/Doc:
;;; https://trac.common-lisp.net/bordeaux-threads/wiki/ApiDocumentation





;;;; Octet Vector and Hex String Utilities

;;; The few operations in this section below build upon the abstraction of the
;;; type octet and octet-vector defined in module EMOTIQ-UTILITIES. These
;;; operations are peripherally related to crypto, but they happen to be
;;; implemented reasonably well in the crypto library we're using for crypto,
;;; Ironclad, so we'll use that library to implement these.


;;; OCTET-VECTOR-TO-HEX-STRING: return a string representing the sequence of
;;; octets of OCTET-VECTOR, a vector with elements of type octet, as a sequence
;;; of hex digit pairs.

(defun octet-vector-to-hex-string (octet-vector)
  (check-type octet-vector octet-vector)
  (ironclad:byte-array-to-hex-string octet-vector))

;;; HEX-STRING-TO-OCTET-VECTOR: return a vector representing the sequence of
;;; hex digit pairs in HEX-STRING, a string, as a vector with elements of type
;;; octet.

(defun hex-string-to-octet-vector (hex-string)
  (ironclad:hex-string-to-byte-array hex-string))

;;; STRING-TO-OCTET-VECTOR: return a vector with elements of type
;;; octet.

(defun string-to-octet-vector (string)
  (ironclad:ascii-string-to-byte-array string))

;; Defer for on string representation issues, UTF-8 vs. 16-bit, Unicode,
;; etc. Ironclad explicitly refuses to deal with it beyond ASCII.





;;;; Crypto

;;; A `digest vector' is an octet vector used to represent a hash result as a
;;; sequence of octets.
;;;
;;; A `digest hex string' is a string used to represent a hash result as a
;;; sequence of pairs of hex digit characters, where each pair is the hex
;;; encoding of a corresponding octet.
;;;
;;; A `digest', abstractly, is an object used to represent a hash result. It may
;;; be either a digest vector or a digest hex string.



;;; A `hashable' is a hashable sequence of data, acceptable as input to the hash
;;; functions below, defined to be either a `hashable string', that is, any
;;; string, or a `hashable vector', that is, a vector with elements of type
;;; (unsigned-byte 8).

;;; NB: currently (Jan 19 2018), if HASHABLE is of type string and contains
;;; other than ASCII printing characters characters, the results are not
;;; guaranteed to be stable and consistent with results in future version of
;;; this software. Some policy decisions shall need to be made that might have
;;; an impact on such cases at a later date.


;;; SHA-256-STRING: return a SHA-256 hash on HASHABLE as a digest hex string of
;;; length 64.
;;;
;;; SHA-256-VECTOR: return a SHA-256 hash of HASHABLE as a digest vector of
;;; length 32.
;;;
;;; SHA3-512-STRING: return a SHA3-512 hash of HASHABLE as a digest hex string
;;; of length 128.
;;;
;;; SHA3-512-VECTOR: return a SHA3-512 hash of HASHABLE as a digest hex string
;;; of length 64.

;;; Examples:
;;;   (sha-256-string "Rosetta code") =>
;;;     "764faf5c61ac315f1497f9dfa542713965b785e5cc2f707d6468d7d1124cdfcf"

(defun sha-256-string (hashable)
  (ironclad-hash hashable :sha256 :string))

(defun sha-256-vector (hashable)
  (ironclad-hash hashable :sha256 :vector))

(defun sha3-512-string (hashable)
  (ironclad-hash hashable :sha3 :string))

(defun sha3-512-vector (hashable)
  (ironclad-hash hashable :sha3 :vector))





;;; IRONCLAD-HASH: hash HASHABLE hash per the method designated by the Ironclad
;;; DIGEST-NAME corresponding to one of our supported hash methods, i.e., either
;;; :sha256 (for SHA-256) or :sha3 (for SHA3-512) and return a digest of
;;; the form specified by DIGEST-TYPE, which can be either :vector to produce a
;;; digest vector or :string to produce a digest hex string. This is
;;; internal to this package, not for export.

(defun ironclad-hash (hashable digest-name digest-type)
  (let* ((digest (ironclad:make-digest digest-name))
         (sequence
           (if (stringp hashable) (string-to-octet-vector hashable) hashable)))
    (ironclad:update-digest digest sequence)
    (let ((hash-digest-vector (ironclad:produce-digest digest)))
      (ecase digest-type
        (:vector hash-digest-vector)
        (:string (octet-vector-to-hex-string hash-digest-vector))))))
