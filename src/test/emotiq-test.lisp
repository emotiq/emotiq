(in-package #:emotiq-test)

;;; Documentation for LISP-UNIT testing framework is here:
;;;
;;;   https://github.com/OdonataResearchLLC/lisp-unit/wiki/Reference
;;;
;;; There's also good documentation here, written by the original author of the
;;; software, although it seems to be a bit out of date:
;;;
;;;   https://www.cs.northwestern.edu/academics/courses/325/readings/lisp-unit.php#usage




;;;; Octet Vector and Hex String Utilities

(defparameter test-hex-string "a0b1c2d3e4f56789")

(define-test octet-vector-to-hex-string
  (let ((octet-vector (hex-string-to-octet-vector test-hex-string)))
    (assert-true (octet-vector-p octet-vector))
    ;; length of octet vector should be 1/2 that of hex string
    (assert-eql (length octet-vector) (floor (length test-hex-string) 2))
    (let ((back-hex-string (octet-vector-to-hex-string octet-vector)))
      (assert-equalp back-hex-string test-hex-string))))


(define-test hex-string-to-octet-vector
  (assert-equal (ovref (hex-string-to-octet-vector "3d") 0) #x3d))
  



;;;; Normalizing Strings as Simple Strings


(defparameter *normalizing-string-test-cases*
  ;; format:
  ;;  ((descriptive-name <string corresponding to description>) ...)
  `((simple-base-string
     ,(make-array 3 :element-type 'base-char :initial-element #\t))
    (test-simple-string
     ,(make-array 3 :element-type 'base-char :initial-element #\t))
    (test-adjustable-string
     ,(make-array
       3 :element-type 'character :initial-element #\s :adjustable t))
    (test-adjustable-string-with-fill-pointer
     ,(make-array
       3 :element-type 'character :initial-element #\t :adjustable t
       :fill-pointer 3))))

#-ccl
#| 

FIXME: 

    CCL complains about the compilation of etypecase usage, but the
    test seems to execute fine.  When someone has the time, please revisit
    with a better understanding of string type hierarchies across implementations.

;Compiler warnings :
;   Clause (SIMPLE-STRING NIL) ignored in ETYPECASE form - shadowed by (BASE-STRING NIL) .
;   Clause (STRING NIL) ignored in ETYPECASE form - shadowed by (BASE-STRING NIL) .
;   Clause (SIMPLE-STRING T) ignored in ETYPECASE form - shadowed by (BASE-STRING NIL) .
;   Clause (STRING NIL) ignored in ETYPECASE form - shadowed by (BASE-STRING NIL) .
;   In an anonymous lambda form: Unused lexical variable DESCRIPTIVE-NAME
|#
(define-test normalizing-strings-as-simple-strings
  (loop for (descriptive-name string)
          in *normalizing-string-test-cases*
        as normalized-simple-base-string
          = (normalize-to-simple-base-string string)
        as normalized-simple-string
          = (normalize-to-simple-string string)
        as normalized-simple-base-string-correctly-p
          = (etypecase normalized-simple-base-string
              (simple-base-string t)
              (base-string nil)
              (simple-string nil)
              (string nil))
        as normalized-simple-string-correctly-p
          = (etypecase normalized-simple-string
              (simple-base-string nil)
              (base-string nil)
              (simple-string t)
              (string nil))
        ;; Test to show that string is EQUAL to its simple-base-string
        ;; and simple-string equivalents, and then make sure
        ;; normalized strings are of exactly the right types.
        do (assert-equal string normalized-simple-base-string)
           (assert-equal string normalized-simple-string)
           (assert-true normalized-simple-base-string-correctly-p)
           (assert-true normalized-simple-string-correctly-p)))
  
  
     




;;;; Crypto


(defparameter string-rosetta-code "Rosetta code")

(define-test sha-256-string
  (let ((digest (sha-256-string string-rosetta-code)))
    (assert-equalp 
     digest
     "764faf5c61ac315f1497f9dfa542713965b785e5cc2f707d6468d7d1124cdfcf")
    (assert-eql (length digest) 64)))

(define-test sha-256-vector
  (let ((digest (sha-256-vector string-rosetta-code)))
    (assert-equalp 
     digest
     #(118 79 175 92 97 172 49 95 20 151 249 223 165 66 113 57 101 183 133 229 204 47 112 125 100 104 215 209 18 76 223 207))
    (assert-eql (length digest) 32)))

(define-test sha3-512-string
  (let ((digest (sha3-512-string string-rosetta-code)))
    (assert-equalp 
     digest   
     "a05395817c9c06a4dfbf88553041463b8a4ace62f438d4f74a61356a66a232b69512544b303be599aa875af1803f3afc2ebe57f1b7a5226717baefc2470d4574")
    (assert-eql (length digest) 128)))

(define-test sha3-512-vector
  (let ((digest (sha3-512-vector string-rosetta-code)))
    (assert-equalp
     digest
     #(160 83 149 129 124 156 6 164 223 191 136 85 48 65 70 59 138 74 206 98 244 56 212 247 74 97 53 106 102 162 50 182 149 18 84 75
       48 59 229 153 170 135 90 241 128 63 58 252 46 190 87 241 183 165 34 103 23 186 239 194 71 13 69 116))
    (assert-eql (length digest) 64)))



;;; Sample ripemd-160 hashes. Source:
;;; https://en.wikipedia.org/wiki/RIPEMD

(defparameter *quick-lazy-dog-to-test-ripemd-160*
  "The quick brown fox jumps over the lazy dog")

(defparameter *quick-lazy-dog-ripemd-160-hash*
  "37f332f68db77bd9d7edd4969571ad671cf9dd3b")

(defparameter *quick-lazy-cog-to-test-ripemd-160*
  "The quick brown fox jumps over the lazy cog")  ; ends with cog, not dog

(defparameter *quick-lazy-cog-ripemd-160-hash*
  "132072df690933835eb8b6ad0b77e7b6f14acad7")

(define-test hash-160-string
  (let (digest)
    (setq digest (hash-160-string *quick-lazy-dog-to-test-ripemd-160*))
    (assert-equalp digest *quick-lazy-dog-ripemd-160-hash*)
    (assert-eql (length digest) 40)
    (setq digest (hash-160-string *quick-lazy-cog-to-test-ripemd-160*))
    (assert-equalp digest *quick-lazy-cog-ripemd-160-hash*)
    (assert-eql (length digest) 40)))



;;; Here is a string of the word hello its sha-256 double hash,
;;; followed by a test for our hash-256-string function. Source:
;;; https://en.bitcoin.it/wiki/Protocol_documentation#Hashes

(defparameter *word-hello-to-test-double-sha-256*
  "hello")

(defparameter *hello-double-sha-256-hash*
  "9595c9df90075148eb06860365df33584b75bff782a510c6cd4883a419833d50")

(define-test hash-256-string
  (let ((digest (hash-256-string *word-hello-to-test-double-sha-256*)))
    (assert-equalp digest *hello-double-sha-256-hash*)
    (assert-eql (length digest) 64)))











