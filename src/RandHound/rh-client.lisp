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
                 (ix    (mod (logxor r (int pkey)) ngrp)))
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

(defun initialization (reply-to purpose &key (max-bft *max-bft*))
  (let* ((vnodes  (get-nodes-vector))
         (nnodes  (length vnodes))
         (nneed   (1+ (* 3 max-bft))))
    (if (< nnodes nneed)
        (error "Not enough servers for BFT")
      ;; else
      (let* ((vnodes   (subseq vnodes 0 nneed))
             (tgrps    (create-server-groups vnodes))
             (config   (construct-session-config vnodes tgrps max-bft purpose))
             (hconfig  (published-form (hash:hash/256 config)))
             (init-msg (make-session-config-message
                        :hash-config hconfig
                        :tgrps       tgrps
                        :purpose     purpose
                        :tstamp      (session-config-tstamp config))))
        (record-to-log config)
        (record-to-log init-msg)
        (broadcast-message (list :randhount-init reply-to init-msg) vnodes)))
    ))

;; --------------------------------------------------------------------
;; Try out Secret Sharing with Pairing-based Crypto
;;
;; G1 = Generator for group G1
;; G2 = Generator for group G2
;; Zr = Generator for field Zr
;; P_i = public key for node i
;; p_i = secret key for node i; P_i = p_i * G
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
;; --------------------------------------------------------------------------
;;
;;              == The "Chinese" Algorithm ==
;;
;;  c.f, "Publicly Verifiable Secret Sharing Schemes Using Bilinear
;;  Pairings", by Tian, Peng, and Ma, International Journal of Network
;;  Security, Vol 14, No. 3, pp 142-148, May 2012
;;
;; The paper suffers from translation to English, and the math
;; nomenclature is inconsistent, as well as in error in one place. I
;; attempt to rederive the notions below:

;; Publish commitments
;;
;;              C_ij = a_ij * G1, j = 0..t-1, all in G1
;;
;; Publish encrypted shares
;;
;;              S_ik = f_i(k) * P_k, k = 1..n, all in G2
;;
;;  (can we use H(P_k) -> Zr for k? lets us assoc responses with P_k)
;; Publish = broadcast to all n nodes: t+n-1 values, t from G1, n-1 from G2
;;
;; Accumlate packets from nodes during timeout period. At end we have
;; m <= n-1 responses, exluding our own. Assert that m >= f for BFT f, else give up.
;; Record which nodes responded, for further broadcast.
;;
;; Verify commitments and shares at node k:
;;
;;   CS_ik = Sum(k^j * C_ij, j = 0..t-1)
;;         = Sum(k^j * a_ij, j = 0..t-1)*G1
;;         = f_i(k)*G1
;;  Check:
;;       e(CS_ik,P_k) = e(G1,S_ik)
;;
;; Compute still-encrypted share:
;;
;;  Y_ik = 1/p_k * S_ik = 1/p_k*f_i(k)*P_k = 1/p_k*f_i(k)*p_k*G2 = f_i(k)*G2
;;
;; Broadcast still-encrypted shares Y_ik and proof CS_ik, to all other
;; m nodes, i in {m}.  Proof of Y_ik is e(CS_ik,G2) = e(G1,Y_ik)
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
;; ---------------------------------------------------------------------------------------
#| ---------------------------------------------------------------------------------------

         ==  Reed-Solomon Code Interpretation of Shamir Sharing ==

   c.f., "SCRAPE: Scalable Randomness Attested by Public Entities", by Cascudo and David

Alternative is to treat list of shares S_ik as code word of Reed-Solomon [n,k,n-k+1].
Instead of publishing proofs on coeffs of polynomial, we publish proofs on the shares S_ik,
and validate by checking for orthogonality in the dual space of the RS Code.

Consider just one node for now:

Pick a random polynomial f(x) of degree t-1 with coeffs a_i. We must
use t = sharing threshold = f + 1 for BFT. Number of nodes n = 3*f + 1.

  f(x) = Sum(x^i * a_i, i = 0 .. t-1), where a_0 = secret value

and

  a_i = Rnd(i)

Let                   x_j = H(P_j) in Zr
Form the shares       s_j = f(x_j) in Zr,
Form encrypted shares S_j = s_j * P_j, all in G2, j = 1..n
Form the proofs       V_j = s_j * G1, all in G1, j = 1..n
Publish shares and proofs to all nodes 1..n

Each node checks share validity by noting that e(V_j,P_j) = e(G1,S_j), j = 1..n
Check that shares form valid RS-Code by computing a random polynomial in the dual space:

    g(x) = Sum(x^i * b_i, i = 0..m <= n-t-1)

and
    b_i = Rnd(i) in Zr

Let  x_j = H(P_j) in Zr, same x_j as used in forming shares,
then form dual vector:

    c_j = d_j * g(x_j), j = 1..n, in Zr

where

    d_j = Prod(1/(j-i),  i /= j, i = 1..n), in Zr

Then show that Sum(c_j * V_j, j = 1..n) = I. This validates the entire
set {S_j} of shares as a valid RS-Code.

Now decrypt your node k share:

   SD_k = 1/p_k*S_k = 1/p_k*s_k*P_k = 1/p_k*s_k*p_k*G2 = s_k*G2, in G2

Publish decrypted share SD_k. Proof of SD_k is e(V_k,G2) = e(G1,SD_k),
V_k already public.

Anyone who sees #SD_k >= t = f+1 can use Lagrange interpolation to
derive final secret S = a_0 * G2.

   S = Sum(lam_j * SD_j, j = 1..m >= t), in G2

where

   lam_j = Prod(j/(j-i), i = 1..m, i /= j), in Zr


Here we choose degree of sharing polynomial f(x) as t-1, making a
[n,t,n-t+1] RS code from (S1,S2,...,Sn). The dual space polynomial
g(x) can have degree n-t-1, making a [n,n-t,t+1] RS code.

[n,m,l] := [length, dimension, distance]

|#
;; ---------------------------------------------------------------------------
#|
       In the end... The "Chinese" algorithm requires the publication of T + N values,
       corresponding to the N Shares and T Proofs of the sharing polynomial coefficients. (T << N)

       The Reed-Solomon requires publication of 2N values corrsponding to N Shares and N Proofs of Shares.

       So opt for the "Chinese" algorithm. Call it TPM for the authors initials.

|#
;; ---------------------------------------------------------------------------

;; Single-thread testing...
(defun gen-randomness (n)
  (let* ((f     (floor (1- n) 3))  ;; number of potential Byzantine failures
         ;; (n     (1+ (* 3 f))) ;; number of nodes = 3*f+1
         (kord  f)            ;; order of sharing polynomial = f, sharing threshold = k+1 = f+1
         (q     (pbc:get-order))
         (coffs (loop repeat (1+ kord) collect
                      (random-between 1 q)))
         (key-pairs (loop for ix from 1 to n collect (make-key-pair ix)))
         (pkeys     (mapcar 'keying-triple-pkey key-pairs))
         (xvals     (mapcar (lambda (pkey)
                              (mod (int pkey) q))
                            pkeys))
         (poly      (lambda (x)
                      (with-mod q
                        (um:nlet-tail iter ((coffs coffs)
                                            (ans   0))
                          (if (endp coffs)
                              ans
                            (iter (cdr coffs)
                                  (m+ (car coffs)
                                      (m* x ans)))
                            )))))
         (shares       (mapcar poly xvals))
         (encr-shares  (mapcar 'expt-pt-zr pkeys shares))
         (coff-proofs  (mapcar (um:curry 'expt-pt-zr (get-g1)) coffs)))
    (list coff-proofs encr-shares key-pairs)))

(defun validate-randomness (lst)
  (destructuring-bind (proofs shares keys) lst
    (let* ((pkeys (mapcar 'keying-triple-pkey keys))
           (skeys (mapcar 'keying-triple-skey keys))
           (q     (get-order))
           (xvals (mapcar (lambda (pkey)
                            (mod (int pkey) q))
                          pkeys))
           (poly  (lambda (x)
                    (let ((xz (make-instance 'zr
                                             :val x)))
                      (um:nlet-tail iter ((pts proofs)
                                          (ans nil))
                        (if (endp pts)
                            ans
                          (iter (cdr pts)
                                (if ans
                                    (mul-pts (car pts)
                                             (expt-pt-zr ans xz))
                                  (car pts)))
                          )))))
           (ver-proofs (mapcar poly xvals)))
      (assert (every (let ((g1  (get-g1)))
                       (lambda (ver pkey share)
                         (let ((p1  (compute-pairing ver pkey))
                               (p2  (compute-pairing g1  share)))
                           (= (int p1) (int p2)))))
                     ver-proofs pkeys shares))
      (list lst
            ver-proofs
            (mapcar (lambda (skey share)
                      (expt-pt-zr share (inv-zr skey)))
                    skeys shares))
      )))

(defun collect-randomness (lst)
  (destructuring-bind ((proofs encr-shares keys) ver-proofs shares) lst
    (declare (ignore proofs encr-shares))
    (assert (every (let ((g1 (get-g1))
                         (g2 (get-g2)))
                     (lambda (ver-proof share)
                       (let ((p1 (compute-pairing ver-proof g2))
                             (p2 (compute-pairing g1        share)))
                         (= (int p1) (int p2)))))
                   ver-proofs shares))
    (let* ((pkeys (mapcar 'keying-triple-pkey keys))
           (q     (get-order))
           (xvals (mapcar (lambda (pkey)
                            (mod (int pkey) q))
                          pkeys)))
      (labels ((lagrange (x)
                 (with-mod q
                   (m/ x
                       (um:nlet-tail iter ((xs xvals)
                                           (prod 1))
                         (if (endp xs)
                             prod
                           (iter (cdr xs)
                                 (let ((diff (m- x (car xs))))
                                   (if (zerop diff)
                                       prod
                                     (m* prod diff))))
                           ))))))
        (um:nlet-tail iter ((shares  shares)
                            (xs      xvals)
                            (ans     nil))
          (if (endp shares)
              ans
            (iter (cdr shares) (cdr xs)
                  (let ((pt (expt-pt-zr (car shares) (lagrange (car xs)))))
                    (if ans
                        (mul-pts ans pt)
                      pt)))
            ))))))

#|
(let* ((nodes      #( 10   19   37   73   145   289   577  1153))
       (times-tpm  #(1.7  3.3  6.5  13.8  31.9  80.0   223  722))
       (times-rs   #(1.6  3.1  6.0  11.9  23.8  47.8   96.8 201)))
  (multiple-value-bind (xmn ywmn slope wsigma niter)
      (linfit:regression (map 'vector 'log nodes)
                         (map 'vector 'log times-tpm)
                         1)
    (print (list :xmn xmn
                 :ywmn ywmn
                 :slope slope
                 :wsigma wsigma
                 :niter niter))
    (plt:plot 'plt nodes times-tpm
              :clear t
              :title "Randomness Generation vs Nbr Participants"
              :xtitle "Nbr Participant Nodes"
              :ytitle "Elapsed Time [sec]"
              :symbol :circle
              :plot-joined t
              :legend "TPM"
              :xlog   t
              :ylog   t)
    (plt:fplot 'plt '(1 2000)
               (lambda (x)
                 (exp (+ ywmn (* slope (- (log x) xmn)))))
               :color :red)
    (plt:plot 'plt nodes times-rs
              :color :blue
              :symbol :circle
              :plot-joined t
              :legend "RS")
    ))
|#

;; ---------------------------------------------------------------------
;; Reed-Solomon Variant

;; Single-thread testing...
(defun gen-randomness-rs (n)
  (let* ((f     (floor (1- n) 3))            ;; number of potential Byzantine failures
         ;; (n     (1+ (* 3 f))) ;; number of nodes = 3*f+1
         (kord  f)            ;; order of sharing polynomial = f, sharing threshold = k+1 = f+1
         (q     (pbc:get-order))
         (coffs (loop repeat (1+ kord) collect
                      (random-between 1 q)))
         (key-pairs (loop for ix from 1 to n collect (make-key-pair ix)))
         (pkeys     (mapcar 'keying-triple-pkey key-pairs))
         (xvals     (um:range 1 (1+ n)))

         (xvals (mapcar (lambda (pkey)
                          (mod (int pkey) q))
                        pkeys))
         
         (poly      (lambda (x)
                      (with-mod q
                        (um:nlet-tail iter ((coffs coffs)
                                            (ans   0))
                          (if (endp coffs)
                              ans
                            (iter (cdr coffs)
                                  (m+ (car coffs)
                                      (m* x ans)))
                            )))))
         (shares       (mapcar poly xvals))
         (encr-shares  (mapcar 'expt-pt-zr pkeys shares))
         (share-proofs (mapcar (um:curry 'expt-pt-zr (get-g1)) shares)))
    (list kord share-proofs encr-shares key-pairs)))

(defun validate-randomness-rs (lst)
  ;; the algorithm presented in the SCRAPE paper for computing the
  ;; dual vector is incorrect. This code does not work...
  (destructuring-bind (k proofs encr-shares keys) lst
    (let* ((pkeys (mapcar 'keying-triple-pkey keys)))
      (assert (every (let ((g1 (get-g1)))
                       (lambda (proof pkey share)
                         (let ((p1 (compute-pairing proof pkey))
                               (p2 (compute-pairing g1    share)))
                           (= (int p1) (int p2)))))
                     proofs pkeys encr-shares))
      (let* ((skeys (mapcar 'keying-triple-skey keys))
             (q     (get-order))
             (n     (length pkeys))
             (kord  (- n k 2))
             (xvals (um:range 1 (1+ n)))

             (pkeys (mapcar 'keying-triple-pkey keys))
             (xvals (mapcar (lambda (pkey)
                              (mod (int pkey) q))
                            pkeys))

             (coffs (loop repeat (1+ kord) collect
                          (random-between 1 q)))
             (invprod (lambda (x)
                        (let ((prod 1))
                          (loop for xi in xvals do
                                (unless (= x xi)
                                  (setf prod (m* prod (m- x xi)))))
                          prod)))
             (poly  (lambda (x)
                      (with-mod q
                        (m/
                         (um:nlet-tail iter ((coffs coffs)
                                             (ans   0))
                           (if (endp coffs)
                               ans
                             (iter (cdr coffs)
                                   (m+ (car coffs)
                                       (m* x ans)))
                            ))
                         (funcall invprod x)
                         ))))
             (cvals  (mapcar poly xvals))
             (chk    (let ((pt  nil))
                       (loop for p in proofs
                             for c in cvals
                             do
                             (let ((c*p  (expt-pt-zr p c)))
                               (setf pt (if pt
                                            (mul-pts pt c*p)
                                          c*p))))
                       pt)))
        (assert (zerop (int chk)))
        (list lst
              (mapcar (lambda (share skey)
                        (expt-pt-zr share (inv-zr skey)))
                      encr-shares skeys))
        ))))

(defun collect-randomness-rs (lst)
  (destructuring-bind ((k proofs encr-shares keys) shares) lst
    (declare (ignore k encr-shares))
    (assert (every (let ((g1 (get-g1))
                         (g2 (get-g2)))
                     (lambda (proof share)
                       (let ((p1 (compute-pairing proof g2))
                             (p2 (compute-pairing g1    share)))
                         (= (int p1) (int p2)))))
                   proofs shares))
    (let* ((pkeys (mapcar 'keying-triple-pkey keys))
           (q     (get-order))
           (xvals (mapcar (lambda (pkey)
                            (mod (int pkey) q))
                          pkeys)))
      (labels ((lagrange (x)
                 (with-mod q
                   (m/ x
                       (um:nlet-tail iter ((xs xvals)
                                           (prod 1))
                         (if (endp xs)
                             prod
                           (iter (cdr xs)
                                 (let ((diff (m- x (car xs))))
                                   (if (zerop diff)
                                       prod
                                     (m* prod diff))))
                           ))))))
        (um:nlet-tail iter ((shares  shares)
                            (xs      xvals)
                            (ans     nil))
          (if (endp shares)
              ans
            (iter (cdr shares) (cdr xs)
                  (let ((pt (expt-pt-zr (car shares) (make-instance 'zr
                                                                    :val (lagrange (car xs))))))
                    (if ans
                        (mul-pts ans pt)
                      pt)))
            ))))))

(defun find-base (n)
  (let ((qm1 (1- (get-order))))
    (um:nlet-tail iter ((m  n))
      (if (and (primes:is-prime? m)
               (zerop (mod qm1 m)))
          m
        (if (> m (* 2 n))
            :none
          (iter (1+ m)))))
    ))

(defun ntt (m coffs &optional inv)
  ;; Number Theoretic Transform - DFT over Finite Field
  ;; n = nbr elements to produce
  ;; m = order of NTT
  ;; coffs = amplitudes for each spectral index
  (let* ((q  (get-order))
         (p  (with-mod q
               (let ((p (m^ 2 (truncate (1- q) m))))
                 (if inv
                     (m/ p)
                   p))))
         (sf (if inv (with-mod q (m/ m)) 1))
         (xs (with-mod q
               (loop for ix from 1 to m
                   for x = 1 then (m* p x)
                   collect x)))
         (poly (lambda (x)
                 (with-mod q
                   (m* sf
                       (um:nlet-tail iter ((cs   (reverse coffs))
                                           (ans  0))
                         (if (endp cs)
                             ans
                           (iter (cdr cs)
                                 (m+ (car cs)
                                     (m* x ans)))
                           ))))))
         (trev (lambda (lst)
                 (cons (car lst) (reverse (cdr lst)))))
         (spec  (mapcar poly xs)))
    (if (and nil inv)
        (funcall trev spec)
      spec)))
         
#|
(let* ((q   (get-order))
       (cs  (loop for ix from 0 to 6 collect
                  (random-between 1 q)))
       (xs  (ntt 43 cs))
       (css (append (make-list (length cs) :initial-element 0)
                    (loop for ix from (length cs) below 43 collect
                          (random-between 1 q))))
       (xsi (ntt 43 xs t))
       (xsii (ntt 43
                  (let ((xs (copy-list xs)))
                    (setf (cadr xs) (with-mod q (m+ 1 (cadr xs))))
                    xs)
                  t))
       (ys  (ntt 43 css))
       (ysi (ntt 43 ys t)))
  #|
  (inspect (with-mod q
             (mapcar 'm- xsi xsii)))
  |#
  (plt:plot 'xx xsi :clear t
            :yrange `(0 ,q)
            :symbol :circle
            :plot-joined t)
  (plt:plot 'xx cs :color :red
            :alpha 0.5
            :symbol :cross
            :plot-joined t)
  (plt:plot 'xx xsii :color :blue
            :symbol :circle
            :plot-joined t)
  (plt:plot 'x css :clear t
            :yrange `(0 ,q)
            :symbol :circle
            :plot-joined t)
  (plt:plot 'x ysi :color :red
            :alpha 0.5
            :symbol :cross
            :plot-joined t)
  (with-mod q
    (reduce 'm+
            (mapcar 'm*  xs cs))))
|#
#|
(let* ((q   (get-order))
       (cs  (loop for ix from 0 to 6 collect
                  (random-between 1 q)))
       (xs  (loop for ix from 1 to 32 collect ix))
       (poly (lambda (x)
               (with-mod q
                 (um:nlet-tail iter ((cs cs)
                                     (ans 0))
                   (if (endp cs)
                       ans
                     (iter (cdr cs)
                           (m+ (car cs)
                               (m* x ans))))
                   ))))
       (invfn (lambda (x)
                (with-mod q
                  (let ((prod 1))
                    (loop for xj in xs do
                          (unless (= xj x)
                            (setf prod (m* prod (m- x xj)))))
                    (m/ prod)))))
       (rinvfn (lambda (x)
                  (let ((prod 1))
                    (loop for xj in xs do
                          (unless (= xj x)
                            (setf prod (* prod (- x xj)))))
                    (/ 300d30 prod))))
       (zs  (mapcar invfn xs))
       (rzs (mapcar rinvfn xs))
       (fzs (ntt 43 zs))
       (ys  (mapcar poly xs))
       (yzs (with-mod q (mapcar 'm* zs ys)))
       (fys (ntt 43 ys)))
  (print (with-mod q (reduce 'm+ yzs)))
  (plt:plot 'xx zs
            ;; (fft:fwd-magnitude-db (coerce rzs 'vector))
            :clear t
            ;; :yrange '(-0.0001 0.0001)
            :symbol :circle
            :plot-joined t))

(let* ((xs (loop for ix from 1 to 31 collect ix))
       (interp (lambda (x xc)
                 (let ((prod 1))
                   (loop for xj in xs do
                         (unless (= xj xc)
                           (setf prod (* prod (/ (- x xj) (- xc xj))))))
                   prod))))
  (plt:fplot 'interp '(1 31)
             (lambda (x)
               (loop for xc from 16 to 16 sum
                     (funcall interp x xc)))
             :clear t
             :yrange '(-2 2)
             ))
|#

(defun tst-fld (n k)
  (let* ((q     (get-order))
         (coffs (loop for ix from 0 below k collect (random-between 1 q)))
         (xs    (loop for ix from 1 to n collect #|ix|# (random-between 1 q)))
         (poly  (lambda (x)
                  (with-mod q
                    (um:nlet-tail iter ((cs  (reverse coffs))
                                        (ans 0))
                      (if (endp cs)
                            ans
                        (iter (cdr cs)
                              (m+ (car cs)
                                  (m* x ans))))))))
         (shares  (mapcar poly xs))
         (bent-shares (let ((shs (copy-list shares)))
                        (setf (cadr shs) (with-mod q
                                           (m+ 1 (cadr shs))))
                        shs))
         (g1      (get-g1))
         (cshares (mapcar (um:curry 'expt-pt-zr g1) shares))
         (bent-cshares (mapcar (um:curry 'expt-pt-zr g1) bent-shares))
         (kdual   (- n k))
         (coffsd  (loop for ix from 0 below kdual collect (random-between 1 q)))
         (invwt   (lambda (x)
                    (with-mod q
                      (um:nlet-tail iter ((xs xs)
                                          (prod 1))
                        (if (endp xs)
                            prod
                          (iter (cdr xs)
                                (let ((xj  (car xs)))
                                  (if (= x xj)
                                      prod
                                    (m* prod (m- x xj)))))
                          )))))
         (polyd    (lambda (x)
                     (with-mod q
                       (m/
                        (um:nlet-tail iter ((cs  (reverse coffsd))
                                            (ans 0))
                          (if (endp cs)
                              ans
                            (iter (cdr cs)
                                  (m+ (car cs)
                                      (m* x ans)))
                            ))
                        (funcall invwt x)))))
         (dualv    (mapcar polyd xs))
         (dotprod  (lambda (v1 v2)
                     (with-mod q
                       (reduce 'm+
                               (mapcar 'm* v1 v2)))))
         (dotprodc (lambda (cv1 v2)
                     (let ((prod  nil))
                       (um:nlet-tail iter ((cs cv1)
                                           (vs v2))
                         (if (endp cs)
                             prod
                           (progn
                             (setf prod (let ((c*v  (expt-pt-zr (car cs) (car vs))))
                                          (if prod
                                              (mul-pts prod c*v)
                                            c*v)))
                             (iter (cdr cs) (cdr vs)))
                           ))))))
    (assert (zerop (funcall dotprod dualv shares)))
    (funcall dotprodc cshares dualv)
    ))