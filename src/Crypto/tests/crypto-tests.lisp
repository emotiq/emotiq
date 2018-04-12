
(in-package :pbc-test)

(define-test keying
  (let ((k (make-key-pair :test)))
    (assert-true (check-public-key (keying-triple-pkey k)
                                   (keying-triple-sig  k)))))

(define-test signature
  (let* ((k  (make-key-pair :test))
         (sig (sign-message "this is a test"
                            (keying-triple-pkey k)
                            (keying-triple-skey k))))
    (assert-true (check-message sig))))

(define-test encryption
  (let* ((msg  "This is a test")
         (k    (make-key-pair :test))
         (cmsg (ibe-encrypt msg (keying-triple-pkey k) :test-id))
         (dmsg (ibe-decrypt cmsg (keying-triple-skey k))))
    (assert-true (string= dmsg msg))))

(define-test multisignature
  (let* ((k1  (make-key-pair :test-1))
         (k2  (make-key-pair :test-2))
         (msg "this is a test")
         (sig1 (sign-message msg
                             (keying-triple-pkey k1)
                             (keying-triple-skey k1)))
         (sig2 (sign-message msg
                             (keying-triple-pkey k2)
                             (keying-triple-skey k2)))
         (msig (combine-signatures sig1 sig2)))
    (assert-true (check-message msig))))

(define-test vrf
  (let* ((k    (make-key-pair :test))
        (vrf  (compute-vrf :test-seed (keying-triple-skey k))))
    (assert-true (validate-vrf vrf (keying-triple-pkey k)))))

(define-test pedersen-proof
  (let* ((proof (make-pedersen-proof 15)))
    (assert-true (validate-pedersen-proof proof))))

(define-test cloaked-proof
  (let* ((proof (make-cloaked-proof 15)))
    (assert-true (validate-cloaked-proof proof))))

(define-test confidential-purchase
  (let* ((kb  (make-key-pair :buyer))
         (kv  (make-key-pair :vendor))
         (cost  100)
         (fees   10)
         (paid  200)
         (change 90)
         ;; make a private transaction between kb and kv
         (purch (confidential-purchase paid change
                                       (keying-triple-pkey kb)
                                       (keying-triple-skey kb)
                                       (keying-triple-pkey kv)))
         ;; make a publicly verifiable transaction from kb
         (ppurch (confidential-purchase paid change
                                        (keying-triple-pkey kb)
                                        (keying-triple-skey kb))))
    (assert-true (check-confidential-purchase purch
                                              cost fees
                                              (keying-triple-skey kv)))
    (assert-true (check-confidential-purchase ppurch
                                              cost fees))))
