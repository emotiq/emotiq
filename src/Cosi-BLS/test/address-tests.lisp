;;;; address-tests.lisp

(in-package :cosi-bls-test)

(define-test public-key-to-key-hash-as-address-test
  (let* ((k                         (make-key-pair :test))
         (public-key                (keying-triple-pkey k))
         (public-key-as-hex-string  (hex-str public-key))
         (public-key-byte-vector    (bev-vec public-key))
         (bitcoin-version           #x00)
         (should-be-bitcoin-address "112TDcS6aiYAG8rRnrM6rijTpiB4MSxnsMo")
         (should-be-mainnet-address "2ZAALAks7kwxXKwfCLKiFLpS3hAmecUpsac") 
         (should-be-testnet-address "1ym7GSk8CpyVoSy61JW4o6QqkTJPMcPHp7T"))
    (print "Public Key to Key Hash as Address Test (Base58Check Encoding)")
    ;; let's really test something here... equivalence of pkey, pkey hex string, and pkey byte vector
    (assert-equal
     (public-key-to-address public-key               :override-version bitcoin-version)
     (public-key-to-address public-key-as-hex-string :override-version bitcoin-version))
    (assert-equal
     (public-key-to-address public-key             :override-version bitcoin-version)
     (public-key-to-address public-key-byte-vector :override-version bitcoin-version))
     ;; stop encouraging the use of untypted bignums and byte vectors...     
    (assert-equal
     should-be-bitcoin-address 
     (public-key-to-address public-key :override-version bitcoin-version)
     'should-be-bitcoin-address)
    (assert-equal 
     should-be-mainnet-address
     (public-key-to-address public-key :net :main)
     'should-be-mainnet-address)
    (assert-equal 
     should-be-testnet-address
     (public-key-to-address public-key :net :test)
     'should-be-testnet-address)))


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
