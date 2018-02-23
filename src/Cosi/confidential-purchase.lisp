;; confidential-purchase.lisp -- Cloaked purchases
;;
;; DM/Emotiq  02/18
;; --------------------------------------------------------------------xs
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

(in-package :crypto-purchase)


;; ------------------------------------------------------------------

(defun rand-val ()
  ;; random value in Z_r
  (random-between (ash *ed-r* -1) *ed-r*))

;; ------------------------------------------------------------------

(defun select-sha3-hash ()
  (let ((nb  (1+ (integer-length *ed-q*))))
    (cond ((> nb 384) :sha3)
          ((> nb 256) :sha3/384)
          (t          :sha3/256)
          )))

(defun sha3-buffers (&rest bufs)
  ;; assumes all buffers are UB8
  (let ((dig  (ironclad:make-digest (select-sha3-hash))))
    (dolist (buf bufs)
      (ironclad:update-digest dig buf))
    (ironclad:produce-digest dig)))

(defun convert-pt-to-v (pt)
  (let ((nb (ceiling (1+ (integer-length *ed-q*)) 8)))
    (convert-int-to-nbytesv (ed-compress-pt pt) nb)))

(defun hash-pt-pt (pt1 pt2)
  (let ((v1 (convert-pt-to-v pt1))
        (v2 (convert-pt-to-v pt2)))
    (convert-bytes-to-int (sha3-buffers v1 v2))))

(defun hash-pts (&rest pts)
  (convert-bytes-to-int
   (apply 'sha3-buffers (mapcar 'convert-pt-to-v pts))))

;; ------------------------------------------------------------------
#|
;; Following along with the ideas presented by Mimblewimble, we
;; arrange a confidential exchange between purchaser and vendor such
;; that the vendor has proof that:
;;
;;   1. the transaction properly covers the cost and fees
;;
;;   2. the purchaser actually produced the purchase request (not a forgery)
;;
;; The purchase transaction contains all the information needed to
;; match up against a price + fee, without revealing actual amounts of
;; currency offered and change requested back. But proof is offered
;; that it covers the vendor's cost and fees.
;;
;; We still need to offer proof that individual currency values are
;; positive or zero, and that they are less than some maximum value.
;; But vendor doesn't need to know that.
;;
;; ----------------------------------------

P = public key ECC pt of purchaser
s = secret key, s*G = P of purchaser
G = generator for ECC curve
k = random blinding term (integer)
K = k*G = corresponding random blinding ECC pt
H(args) = Hash function

Customer:
msg = (change - paid)*P + k*G,
c = H(k*G,(change - paid)*P,P)
r = k - c*s
(msg r P) => vendor

Vendor:
A = (cost + fees)*P
K = msg + A
K =?= r*G + H(K,-A,P)*P

This works because (change - paid) + (cost + fees) = 0 for valid
purchase transactions.

What about a dishonest vendor who accepts anything without checking? 
  1. It would seem suicidal because attackers could forge bogus messages,
     and any attacker would likely cheat the vendor to gain as much for himself 
     as possible.
  2. The output UTXO from the vendor wouldn't check and would be rejected.
     And so, it wouldn't really help the attacker either. Nor would it damage
     the implied customer victim (from PKey).
|#

;; Client side -- create a purchase order
(defun make-purchase-transaction (order-id cost fees paid skey pkey)
  ;; By simply tallying a series of such purchase orders, you will
  ;; know the final account value. Starting with value V0, purch order
  ;; provides (Change - V0), which when added to V0 leaves Change as
  ;; account value.
  (check-type cost (integer 0))
  (check-type fees (integer 0))
  (check-type paid (integer 0))
  (check-type skey (integer 1)) ;; the private key
  (check-type pkey (integer 1)) ;; a compressed ECC pt = public key
  (let ((change (- paid (+ cost fees))))
    (declare (integer change))
    
    (assert (not (minusp change))) ;; we will have to provide proof that change >= 0

    (let* ((p       (ed-decompress-pt pkey))
           (k       (rand-val))
           (kpt     (ed-nth-pt k))
           (msg-kpt (ed-mul p (- change paid)))
           (c       (hash-pts kpt msg-kpt p))
           (r       (sub-mod *ed-r* k (mult-mod *ed-r* c skey)))
           (msg     (ed-compress-pt (ed-add msg-kpt kpt))))
      (declare (integer k c r))
      (list :purchase
            order-id
            msg
            r
            pkey))))

;; Vendor side - verify purchase order, and if it checks out okay,
;; construct UTXO's for the purchaser's change, the vendor's income
;; from sale, and the fees paid to whomever.
(defun check-purchase-transaction (purch cost fees)
  (check-type cost (integer 0))
  (check-type fees (integer 0))
  (destructuring-bind (hdr order-id msg r pkey) purch
    (declare (ignore hdr order-id)
             (integer r))
    (let* ((msg  (ed-decompress-pt msg))
           (pkey (ed-decompress-pt pkey))
           (apt  (ed-mul pkey (+ cost fees)))
           (kpt  (ed-add msg apt))
           (c    (hash-pts kpt (ed-negate apt) pkey))
           (chk  (ed-add (ed-nth-pt r)
                         (ed-mul pkey c))))
      (declare (integer c))
      (when (ed-pt= kpt chk)
        (list ;; list of 3 UTXO's on output
         ;; create a (change - paid) transaction to add to purchaser's account
         (list :utxo      
               (gen-uuid-int) ;; timestamp
               msg r c pkey) ;; enough info to derive blinding K
         ;; generate a similar UTXO for vendor 
         (gen-utxo-for-me cost)
         ;; and generate a similar UTXO for fee payment
         (gen-fee fees)))
      )))

(defun NYI (arg)
  (error "Not yet implemented: ~A" arg))

(defun gen-fee (&rest args)
  (declare (ignore args))
  (NYI :gen-fee))

(defun gen-uuid-int ()
  (uuid:uuid-to-integer (uuid:make-v1-uuid)))

(defun gen-utxo-for-me (&rest args)
  (declare (ignore args))
  (NYI :gen-utxo-for-me))

