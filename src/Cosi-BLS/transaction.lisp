
(in-package :cosi/proofs)

;; Pedersen commitment independent generators on Curve-1174
(defvar *ped-a*  (make-ecc-pt
                  :X 1130135147990688640681743865112702981127711051993170049139417907442520187
                  :Y 3041026309257455567691698424470438178325291934439621627453008713123257100648))

(defvar *ped-b*  (make-ecc-pt
                  :X 2238759581034310242319466026071642911397031369300584047294277274522848286488
                  :Y 439780329773957263333761663678961987026660243444010341003172863159608452492))

(defun rand ()
  "Return a random integer from Z_r over the domain of the curve
field. 1 <= z_rand < group order"
  (random-between 1 *ed-r*))

;; --------------------------------------------------------------------

(defclass pedersen-commitment ()
  ((c     :reader  pedersen-cmt-c
          :initarg :c)
   (l     :reader  pedersen-cmt-l
          :initarg :l)
   (r     :reader  pedersen-cmt-r
          :initarg :r)
   (z     :reader  pedersen-cmt-z
          :initarg :z)
   (alpha :reader  pedersen-cmt-alpha
          :initarg :alpha)
   ))

(defclass pedersen-secrets ()
  ((gamma :reader  pedersen-secr-gamma
          :initarg :gamma)
   (x     :reader  pedersen-secr-x
          :initarg :x)))

;; --------------------------------------------------------------------

(defun make-pedersen-commitment (x)
  "Construct a Pedersen commitment to x. This is a computationally
binding, hiding, commitment.

   C = gamma * A + x * B

for A, B, C points on an Elliptic Curve. Factor gamma is a random
hiding factor, and x is the value being committed.

Publish A, B, C, along with L, R points. Compute Fiat-Shamir
challenge, z, as the hash of the public record:

   z = Hash(C, L, R, A, B)

for compressed points C, L, R, and uncompressed constant generators A,
B. No known relationship between A, B. We use SHA-3/256 for Hash.

Return

    alpha(z) = z * gamma + x/z

So that

    G' = (1/z) * A + z * B

    alpha * G' = (1/z^2) * L + C + z^2 * R
"
  (with-mod *ed-r*
    (let* ((gamma  (rand))
           (cmt    (ed-compress-pt
                    (ed-add (ed-mul *ped-a* gamma)
                            (ed-mul *ped-b* x))))
           (lf     (ed-compress-pt (ed-mul *ped-a* x)))
           (rt     (ed-compress-pt (ed-mul *ped-b* gamma)))
           (z      (int (hash:hash/256 cmt lf rt *ped-a* *ped-b*)))
           (alpha  (m+ (m* gamma z)
                       (m/ x     z))))
      (values
       (make-instance 'pedersen-commitment
                      :c  cmt
                      :l  lf
                      :r  rt
                      :z  z
                      :alpha alpha)
       (make-instance 'pedersen-secrets
                      :gamma  gamma
                      :x      x))
      )))

(defmethod validate-pedersen-commitment ((pc pedersen-commitment))
  (with-accessors ((cmtc  pedersen-cmt-c)
                   (lfc   pedersen-cmt-l)
                   (rtc   pedersen-cmt-r)
                   (z     pedersen-cmt-z)
                   (alpha pedersen-cmt-alpha)) pc
    (when (eql z (int (hash:hash/256 cmtc lfc rtc *ped-a* *ped-b*)))
      (with-mod *ed-r*
        (let* ((gzpt  (ed-add (ed-mul *ped-a* (m/ z))
                              (ed-mul *ped-b* z)))
               (cmt   (ed-decompress-pt cmtc))
               (lf    (ed-decompress-pt lfc))
               (rt    (ed-decompress-pt rtc))
               (zsq   (m* z z)))
          (= (int (ed-compress-pt (ed-mul gzpt alpha)))
             (int (ed-compress-pt (ed-add (ed-mul lf (m/ zsq))
                                          (ed-add  cmt
                                                   (ed-mul rt zsq)))
                                  )))
          )))))

;; --------------------------------------------------------------------

(defclass cloak-proof ()
  ((cmtx   :reader  cloak-proof-cmtx
           :initarg :cmtx)
   (aadj   :reader  cloak-proof-aadj
           :initarg :aadj)
   (pedk   :reader  cloak-proof-pedk
           :initarg :pedk)
   (pedxmk :reader  cloak-proof-pedxmk
           :initarg :pedxmk)))

(defclass cloak-secrets ()
  ((k-secr   :reader  cloak-secrets-k-secr
             :initarg :k-secr)
   (xmk-secr :reader  cloak-secrets-xmk-secr
            :initarg :xmk-secr)
   (x        :reader  cloak-secrets-x
             :initarg :x)
   (gamm-x   :reader  cloak-secrets-gamm-x
             :initarg :gamm-x)))

(defun make-cloak-proof (x &optional (gam-x (rand)))
  (with-mod *ed-r*
    (multiple-value-bind (ped-k sec-k)
        (make-pedersen-commitment (rand))   ;; commit to cloaking, k
      (let ((k     (pedersen-secr-x sec-k)) ;; random cloaking factor
            (gam-k (pedersen-secr-gamma sec-k)))
        (multiple-value-bind (ped-xmk sec-xmk)
            (make-pedersen-commitment (m- x k))
          (let* ((gam-xmk  (pedersen-secr-gamma sec-xmk))
                 (aadj     (m- gam-x (m+ gam-xmk gam-k)))
                 (cmt-x    (ed-add (ed-mul *ped-a* gam-x)
                                   (ed-mul *ped-b* x))))
            (values
             (make-instance 'cloak-proof
              :cmtx   (ed-compress-pt cmt-x)
              :aadj   aadj
              :pedk   ped-k
              :pedxmk ped-xmk)
             (make-instance 'cloak-secrets
              :k-secr   sec-k
              :xmk-secr sec-xmk
              :x        x
              :gamm-x   gam-x))
            ))))))


(defmethod validate-cloaked-proof ((prf  cloak-proof))
  (with-accessors ((cmt-x  cloak-proof-cmtx)
                   (aadj   cloak-proof-aadj)
                   (pedk   cloak-proof-pedk)
                   (pedxmk cloak-proof-pedxmk)) prf
    (when (and (validate-pedersen-commitment pedk)
               (validate-pedersen-commitment pedxmk))
      (with-accessors ((cmt-k  pedersen-cmt-c)) pedk
        (with-accessors ((cmt-xmk pedersen-cmt-c)) pedxmk
          (with-mod *ed-r*
            (= (int cmt-x)
               (int (ed-compress-pt
                     (ed-add (ed-mul *ped-a* aadj)
                             (ed-add (ed-decompress-pt cmt-xmk)
                                     (ed-decompress-pt cmt-k)))))
               )))))))
               
;; ------------------------------------------------------------------------------

(defclass value-proof ()
  ((clk-proof  :reader  value-clk-proof ;; a cloak proof
               :initarg :clk)
   (rng-proof  :reader  value-rng-proof ;; a Bulletproof
               :initarg :rng)))

(defun make-value-proof (x &optional gam)
  (assert (and (<= 0 x)
               (<= (integer-length x) 64)))
  (multiple-value-bind (clk-proof clk-secr)
      (make-cloak-proof x gam)
    (values
     (make-instance 'value-proof
                    :clk  clk-proof
                    :rng  (range-proofs:make-range-proofs 64 x))
     clk-secr))) ;; cloaking secrets

(defmethod validate-value-proof ((prf value-proof))
  (with-accessors ((clk-proof   value-clk-proof)
                   (rng-proof   value-rng-proof)) prf
    (and (validate-cloaked-proof clk-proof)
         (range-proofs:validate-range-proofs rng-proof))))

(defmethod cmt-val ((prf value-proof))
  "Locate the Pedersen commitment on the uncloaked value itself"
  (cloak-proof-cmtx (value-clk-proof prf)))

;; ----------------------------------------------------------------------------------
  
(defclass utx-spend ()
  ((hashlock  :reader  utx-spend-hashlock ;; h = H(C, P)
              :initarg :hashlock)
   (cmt       :reader  utx-spend-cmt      ;; C
              :initarg :cmt)
   (pkey      :reader  utx-spend-pkey     ;; P
              :initarg :pkey)
   (sig       :reader  utx-spend-sig      ;; Sig(h, P)
              :initarg :sig))
  (:documentation "The 4-tuple that represents the spend side of a simple transaction"))

(defclass utx-uncloaked-send ()
  ((amt   :reader  utx-uncloaked-send-amt
          :initarg :amt)))

(defclass utx-send ()
  ((hashpkey  :reader  utx-send-hashpkey ;; hash of recipient's public key
              :initarg :hashpkey)
   (hashlock  :reader  utx-send-hashlock ;; h = H(C, P) -- this is the UTX ID
              :initarg :hashlock)
   (cmt       :reader  utx-send-cmt      ;; C - committment proof on sent amount
              :initarg :cmt)
   (encr      :reader  utx-send-encr     ;; encrypted info for future spending of UTX
              :initarg :encr))
  (:documentation "The pair that represents a send UTX"))

(defclass utx-send-secrets ()
  ((pkey      :reader  utx-send-secr-pkey ;; pkey used for this receipt, hash to help locate UTX
              :initarg :pkey)
   (amt       :reader  utx-send-secr-amt  ;; form commitment C = gam*A + amt*B to form
              :initarg :amt)              ;;  the hashlock h = Hash(C, P) = UTX ID
   (gam       :reader  utx-send-secr-gam  ;; use this hiding factor when you re-make the commitment
              :initarg :gam))
  (:documentation "This is the stuff needed by recipient to be able to reconstruct a spend proof.
It should be stored in the wallet"))

(defmethod make-hashlock ((prf value-proof) (pkey pbc:public-key))
  "We are often given a long value proof, and we need the commitment
to the uncloaked value"
  (hash:hash/256 (cmt-val prf) pkey))

(defun make-utx-spend (amt gam pkey skey)
  "Make a spend UTX with value proof"
  (let* ((prf       (make-value-proof amt gam))
         (hashlock  (make-hashlock prf pkey))
         (sig       (pbc:sign-hash hashlock skey)))
    (values 
     (make-instance 'utx-spend
                    :hashlock  hashlock
                    :cmt       prf
                    :pkey      pkey
                    :sig       sig)
     gam)))

#|
(let* ((k    (pbc:make-key-pair :dave))
       (pkey (pbc:keying-triple-pkey k))
       (skey (pbc:keying-triple-skey k))
       (utx  (make-utx-spend 1000 1 pkey skey)))
  ;; (inspect utx)
  (validate-spend-utx utx)
  )
 |#

(defmethod make-utx-send ((amt integer) (pkey pbc:public-key))
  "Make a cloaked send with value proof"
  (multiple-value-bind (prf secr) (make-value-proof amt)
    (let* ((gam  (cloak-secrets-gamm-x secr))
           (info (make-instance 'utx-send-secrets
                                :pkey  pkey
                                :amt   amt
                                :gam   gam)))
    (values
     (make-instance 'utx-send
                    :hashpkey  (hash:hash/256 pkey)     ;; for recipient to locate tokens
                    :hashlock  (make-hashlock prf pkey) ;; the UTX ID
                    :cmt       prf                      ;; value proof
                    :encr      (pbc:ibe-encrypt (loenc:encode info) pkey :spend-info))
     info)
    )))

(defmethod make-utx-send ((amt integer) (pkey null))
  "Make an uncloaked send with raw value showing"
  (make-instance 'utx-uncloaked-send
                 :amt  amt))

(defmethod utx-send-cmt ((utx utx-uncloaked-send))
  (ed-compress-pt (ed-mul *ped-b* (utx-uncloaked-send-amt utx))))

;; ------------------------------------------------------------------------------

(defclass transaction ()
  ((spends  :reader  trans-spend-utxs ;; utx-spend from current owner
            :initarg :spends)
   (sends   :reader  trans-send-utxs  ;; utx-send or integer, to new owner or self
            :initarg :sends)
   (gamadj  :reader  trans-gamma      ;; A curve adjustment to get to zero balance.
            :initarg :gamma) 
   ))

(defun make-transaction (spends gam-spends sends send-secrets)
  "Spends is a list of utx-spend, sends is a list of utx-sends, some
cloaked, some not.  Add up the spends, subtract the sends. Result
should be zero value, but some non-zero gamma sum. We make a
correction factor gamma on curve A for the overall transaction."
  (let* ((gam-sends  (mapcar 'utx-send-secr-gam send-secrets))
         (gamma      (with-mod *ed-r*
                       (m- (reduce 'm+ gam-sends)
                           (reduce 'm+ gam-spends)))))
    (make-instance 'transaction
                   :spends  spends
                   :sends   sends
                   :gamma   gamma)))


(defun decrypt-spend-info (encr skey)
  (pbc:ibe-decrypt encr skey))

(defmethod validate-spend-utx ((utx utx-spend))
  (let* ((hl  (make-hashlock (utx-spend-cmt utx)
                             (utx-spend-pkey utx))))
    (and (= (int hl)
            (int (utx-spend-hashlock utx)))
         (pbc:check-hash hl
                         (utx-spend-sig utx)
                         (utx-spend-pkey utx))
         (validate-value-proof (utx-spend-cmt utx))
         )))

(defmethod validate-send-utx ((utx utx-send))
  (validate-value-proof (utx-send-cmt utx)))

(defmethod validate-send-utx ((utx utx-uncloaked-send))
  t)

(defmethod validate-transaction ((trn transaction))
  "Validate amounts and zero balance of transaction, not double spending check"
  (with-accessors ((spends  trans-spend-utxs)
                   (sends   trans-send-utxs)
                   (gamma   trans-gamma)) trn
    (when (and (every 'validate-spend-utx spends)
               (every 'validate-send-utx sends))
      (let* ((cspends  (mapcar 'ed-decompress-pt
                               (mapcar 'cmt-val (trans-spend-utxs trn)
                                       :key 'utx-spend-cmt)))
             (csends   (mapcar 'ed-decompress-pt
                               (mapcar 'cmt-val (trans-send-utxs trn)
                                       :key 'utx-send-cmt)))
             (tspend   (reduce 'ed-add cspends
                               :initial-value (ed-neutral-point)))
             (tsend    (reduce 'ed-add csends
                               :initial-value (ed-neutral-point))))
        (ed-neutral-point-p (ed-add (ed-mul *ped-A* gamma)
                                    (ed-sub tspend tsend)))
        ))))

        
