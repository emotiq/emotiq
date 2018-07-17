
(in-package :crypto/test)

(defun chk-pairing-curves-fn ()
  (and
   (hash-check pbc::*curve-fr449-params*   pbc::*chk-curve-fr449-params*)
   (hash-check pbc::*curve-fr256-params*   pbc::*chk-curve-fr256-params*)))

(define-test chk-pairings-curves
  (assert-true (chk-pairing-curves-fn)))

(define-test chk-keying
  (let ((k (make-key-pair :test)))
    (assert-true (check-public-key (keying-triple-pkey k)
                                   (keying-triple-sig  k)))))

(define-test chk-signature
  (let* ((k    (make-key-pair :test))
         (sig  (sign-hash :test-contents (keying-triple-skey k))))
    (assert-true (check-hash :test-contents sig (keying-triple-pkey k)))))
    
(define-test chk-encrypt
  (let* ((msg  :This-is-a-test)
         (k    (make-key-pair :test))
         (cyph (ibe-encrypt msg (keying-triple-pkey k) :check-encryption)))
    (assert-true (eql msg (ibe-decrypt cyph (keying-triple-skey k))))))

(define-test chk-pairing
  (let* ((u   (get-g1))
         (v   (get-g2))
         (r   (field-random (get-order)))
         (pt1 (mul-pt-zr u r))
         (pt2 (mul-pt-zr v r))
         (p1  (compute-pairing pt1 v))
         (p2  (compute-pairing u pt2)))
    (assert-true (int= p1 p2))))
    
#|
(lisp-unit:run-tests '(chk-pairing) :crypto/test)
(lisp-unit:run-tests :all :crypto/test)
|#
