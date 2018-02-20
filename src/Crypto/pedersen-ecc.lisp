;; pedersen-ecc.lisp - a look at Pedersen Commitments
;;
;; DM/Emotiq  01/18
;; ------------------------------------------------------------------
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

;; Needs Edwards.lisp compiled and loaded...
(user::asdf :core-crypto)

(defpackage :pedersen-ecc
  (:use :common-lisp)
  (:import-from :ecc-crypto-b571
   :ed-mul
   :ed-add
   :ed-sub
   :ed-div
   :ed-affine
   :*edcurve*
   :*ed-r*
   :*ed-gen*
   :ed-curve-name
   :random-between
   :ed-pt=
   :ed-compress-pt
   :ed-decompress-pt
   :sha3-buffers
   :convert-bytes-to-int
   :with-ed-curve
   :*curve-e521*
   )
  (:export
   ))

(in-package :pedersen-ecc)

(defun rand-val ()
  ;; random value in Z_r
  (with-ed-curve *curve-e521*
    (random-between 1 *ed-r*)))

(defun hashem-int (&rest args)
  (convert-bytes-to-int
   (apply 'sha3-buffers args)))

(defstruct commitment
  ;; points stored in compressed form for network transmission
  curve  ;; the ECC curve
  gens   ;; the ECC generators used for dot-prod basis
  hgen   ;; random point on curve
  com    ;; the commitment value (a point on the curve)
  )

(defun commit-vector (a-vec g-vec h-gen blinding-s)
  ;; h-gen = (pf-expt *pf-rg* secret-a)
  ;; keep secrets: secret-a, blinding-s
  (with-ed-curve *curve-e521*
    (make-commitment
     :curve  (ed-curve-name *edcurve*)
     :gens   (mapcar 'ed-compress-pt g-vec)
     :hgen   (ed-compress-pt h-gen)
     :com    (ed-compress-pt
              ;; s*h + (a . g), for a, g vectors
              (ed-add
               ;; shield dot prod with blinding
               (ed-mul h-gen blinding-s)
               ;; compute g^(alpha .dot. a)
               (reduce 'ed-add
                       (mapcar 'ed-mul g-vec a-vec))))
     )))

;; ---------------------------------------------------------------
;; COMMIT - For a vector of values a-vec, create a Pedersen Commitment
;;
;; This current version effectively forms the dot-product between
;; a-vec and a random blinding vector formed as g-vec. When ready to
;; reveal the commitment, share the a-vec and the random blinding-s
;; value formed during the commitment.
;;
(defun commit (a-vec)
  ;; send com portion, keep secret-a & blinding-s secret for now
  ;; when ready to reveal the commited values, send a-vec and blinding-s
  (with-ed-curve *curve-e521*
    (let* ((secret-a   (rand-val)) ;; never shared
           (blinding-s (apply 'hashem-int secret-a a-vec)) ;; (rand-val))
           (g-vec      (make-gens (length a-vec)))
           (h-gen      (ed-mul *ed-gen* secret-a))
           (com        (commit-vector a-vec g-vec h-gen blinding-s)))
      (list :com com ;; public share
            :secret-a secret-a  ;; keep secret
            :blinding-s blinding-s)))) ;; keep secret
  
  ;; -------------------------------------------------------------------

(defun make-gens (nel)
  ;; form a random blinding vector of generators g^alpha_i
  ;; ensure no duplicates (very small chance of that)
  (with-ed-curve *curve-e521*
    (um:nlet-tail iter ((nel nel)
                        (ans nil))
      (if (plusp nel)
          (let ((g-alpha (ed-mul *ed-gen* (rand-val))))
            (if (member g-alpha ans :test 'ed-pt=)
                (iter nel ans)
              (iter (1- nel) (cons g-alpha ans))))
        ans))))

(defun make-coffs (nel)
  ;; choose some random values for test a-vec
  (loop repeat nel collect
        (rand-val)))

#|
;; try it out... 
(let* ((nel  5)
       (av   (make-coffs nel)))
  (inspect (commit av)))
 |#
