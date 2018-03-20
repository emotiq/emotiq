;; rh-client.lisp -- Randhound Client
;;
;; DM/Emotiq  03/18
;; ---------------------------------------------------------------
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

(in-package :randhound/client)

;; ---------------------------------------------------------------

(defun create-server-groups (vnodes)
  ;; divide servers into groups based on hash of node ID (pkey) and
  ;; random seed
  ;;
  ;; With N available servers, divide into Sqrt(N) groups, each with
  ;; approx Sqrt(N) servers / group
  ;;
  (let* ((nel    (length vnodes))
         (ngrp   (isqrt nel))
         (tgrp   (make-array ngrp))   ;; vector of server lists
         (r      (ctr-drbg-int 256))) ;; random seed
    (loop for node across vnodes do
          ;; cheap hash is xor
          (let* ((pkey  (node-assoc-pkey node))
                 (ix    (mod (logxor r (need-integer-form pkey)) ngrp)))
            (push pkey (aref tgrp ix))))
    ;; return vector of groups, each group a list of pkeys
    tgrp))

(defstruct session-config
  pkeys tgrps max-bft purpose tstamp)

(defun construct-session-config (vnodes tgrps max-bft purpose)
  (make-session-config
   :pkeys   (map 'vector 'node-assoc-pkey vnodes)
   :tgrps   tgrps
   :max-bft max-bft
   :purpose purpose
   :tstamp  (get-timestamp)))

(defstruct session-config-message
  hash-config tgrps purpose tstamp)

(defun initialization (purpose &key (max-bft *max-bft*))
  (let* ((vnodes  (get-nodes-vector))
         (nnodes  (length vnodes))
         (nneed   (1+ (* 3 max-bft))))
    (if (< nnodes nneed)
        (error "Not enough servers for BFT")
      ;; else
      (let* ((vnodes   (subseq vnodes 0 nneed))
             (tgrps    (create-server-groups vnodes))
             (config   (construct-session-config vnodes tgrps max-bft purpose))
             (hconfig  (published-form (sha3/256-buffers config)))
             (init-msg (make-session-config-message
                        :hash-config hconfig
                        :tgrps       tgrps
                        :purpose     purpose
                        :tstamp      (session-config-tstamp config))))
        (record-to-log config)
        (record-to-log init-msg)
        (broadcast-message init-msg vnodes)))
    ))

;; --------------------------------------------------------------------
;; Try out Secret Sharing with Pairing-based Crypto
;;
;; G1 = Generator for group G1
;; G2 = Generator for group G2
;; Zr = Generator for field Zr
;; P_i = public key for node i
;; s_i = secret key for node i; P_i = s_i * G
;; e(A, B) = pairing between A in G1, B in G2;
;;       => e(s_i*H(P_i), G) = e(H(P_i), s_i*G) = e(H(P_i),P_i)
;; H(x) = hash of x impressed on field
;; Rnd(x) = random value with seed x
;; n = nbr nodes
;; t = threshold for secret sharing
;;
;; Each node i computes a random polynomial f_i(x) of degree t-1 with coeffs a_ij:
;;
;;    f_i(x) = Sum(x^j * a_ij, j = 0..t-1)
;;
;;    a_ij = Rnd(i|j)
;;
;; Node's secret value will be a_i0.
;;
;; Publish commitments C_ij = a_ij * G1, j = 0..t-1, all in G1
;; Publish encrypted shares S_ik = f_i(k) * P_k, k = 1..n, all in G2
;;  (can we use H(P_k) -> Zr for k? lets us assoc responses with P_k)
;; Publish = broadcast to all n nodes: t+n-1 values, t from G1, n-1 from G2
;;
;; Accumlate packets from nodes during timeout period. At end we have
;; m <= n-1 responses, exluding our own. Assert that m >= f for BFT f, else give up.
;; Record which nodes responded, for further broadcast.
;;
;; Verify commitments and shares at node k:
;;
;;   C_i = Sum(k^j * C_ij, j = 0..t-1)
;;       = Sum(k^j * a_ij, j = 0..t-1)*G1
;;       = f_i(k)*G1
;;  Check:
;;       e(C_i,P_k) = e(G1,S_ik)
;;
;; Compute still-encrypted share:
;;
;;  Y_ik = 1/s_k * S_ik = 1/s_k*f_i(k)*P_k = 1/s_k*f_i(k)*s_k*G2 = f_i(k)*G2
;;
;; Broadcast still-encrypted shares Y_ik to all other m nodes, i in {m}:
;;
;; Receive at least t-1 other still-encrypted shares Y_ij, for total of t shares
;;
;; Solve for S_i0 with Lagrange interpolation. For each node (incl our
;; own) for which we have >= t responses, label them as j = 1..p:
;;
;;  S_j = Sum(Y_jk, k = 1..p), j = 1..t
;;
;; this adds the individual polynomials together.
;;
;;  S_0 = Sum(S_j * lam_j, j = 1..t) where lam_j is Lagrange coeff Prod(j/(j-i), j /= i, j = 1..t)
;;
;; But this is still a G2 value. We could use it straight away, in
;; compressed form, or else hash it, as the shared secret value.
;;
;;
