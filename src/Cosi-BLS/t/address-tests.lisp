;;;; address-tests.lisp

(in-package :cosi-tests)

(define-test public-key-to-key-hash-as-address-test
  (let* ((public-key-as-hex-string
           "0433F8C523B3FF52F0A515DD19EB88B1356BED642F5B9A55AE34D7481FE2EED2D36BDACAFD1A400910CDD1F3BB79A8C4D090C37180156BE25D2801D53DFA646066")
         (public-key-byte-vector
           (ironclad:hex-string-to-byte-array public-key-as-hex-string))
         (bitcoin-version #x00)
         (should-be-bitcoin-address
           "1ABD7Te3tqtMmdmYh432fSyB2fX3juS475")
         (should-be-mainnet-address
           "2ZHtKfnQawFgcxizxB2eRHXwQ1fEM62RSiT")
         (should-be-testnet-address
           "ytqFwmfg1HDu5kRm9Czy38M6mnr45QaA3L"))
    (print "Public Key to Key Hash as Address Test (Base58Check Encoding)")
    (assert-equal 
     should-be-bitcoin-address 
     (public-key-to-address
      public-key-byte-vector :override-version bitcoin-version))
    (assert-equal 
     should-be-mainnet-address
     (public-key-to-address public-key-byte-vector :net :main))
    (assert-equal 
     should-be-testnet-address
     (public-key-to-address public-key-byte-vector :net :test))))


;; Test notes: it may be helpful to test individual components of the
;; address.lisp code with the handy address tester here:
;;
;;   http://gobittest.appspot.com/Address 
;;
;; E.g., you can copy/paste what's in the blank labelled "1 - Public ECDSA Key"
;; into a Lisp string assigned to, say, variable *S*, and then you can do
;;
;;   (public-key-to-address (ironclad:hex-string-to-byte-array *s*) :override-version 0)
;;
;; Try with *S* = "0433F8C523B3FF52F0A515DD19EB88B1356BED642F5B9A55AE34D7481FE2EED2D36BDACAFD1A400910CDD1F3BB79A8C4D090C37180156BE25D2801D53DFA646066"
;; Result should be: "1ABD7Te3tqtMmdmYh432fSyB2fX3juS475"
;;
;; You can also put the above (or some other) public key into blank labelled "1
;; - Public ECDSA Key" at the test site and click the "send" button to verify
;; that it gets the same result.
