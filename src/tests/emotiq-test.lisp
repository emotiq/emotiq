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

