
(in-package :cosi/proofs)

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

(defmethod make-hashlock ((prf range-proofs:range-proof) (pkey pbc:public-key))
  "We are often given a long value proof, and we need the commitment
to the uncloaked value"
  (hash:hash/256 (range-proofs:proof-simple-commitment prf) pkey))

(defun make-utx-spend (amt gam pkey skey)
  "Make a spend UTX with value proof"
  (let* ((prf       (range-proofs:make-range-proof amt :gamma gam))
         (hashlock  (make-hashlock prf pkey))
         (sig       (pbc:sign-hash hashlock skey)))
    (values 
     (make-instance 'utx-spend
                    :hashlock  hashlock
                    :cmt       prf
                    :pkey      pkey
                    :sig       sig)
     gam)))

(defmethod make-utx-send ((amt integer) (pkey pbc:public-key))
  "Make a cloaked send with value proof"
  (multiple-value-bind (prf gam)
      (range-proofs:make-range-proof amt)
    (let ((info  (make-instance 'utx-send-secrets
                                :pkey  pkey
                                :amt   amt
                                :gam   gam)))
      (values
       (make-instance 'utx-send
                      :hashpkey  (hash:hash/256 pkey)     ;; for recipient to locate tokens
                      :hashlock  (make-hashlock prf pkey) ;; the UTX ID
                      :cmt       prf                      ;; value proof
                      :encr      (pbc:ibe-encrypt (loenc:encode info) pkey :spend-info))
       info))))

(defmethod make-utx-send ((amt integer) (pkey null))
  "Make an uncloaked send with raw value showing"
  (make-instance 'utx-uncloaked-send
                 :amt  amt))

(defmethod utx-send-cmt ((utx utx-uncloaked-send))
  (ed-compress-pt (ed-nth-pt (utx-uncloaked-send-amt utx))))

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
         (range-proofs:validate-range-proof (utx-spend-cmt utx))
         )))

(defmethod validate-send-utx ((utx utx-send))
  (range-proofs:validate-range-proof (utx-send-cmt utx)))

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
                               (mapcar (um:compose 'range-proofs:proof-simple-commitment
                                                   'utx-spend-cmt)
                                       (trans-spend-utxs trn))))
             (csends   (mapcar 'ed-decompress-pt
                               (mapcar (um:compose 'range-proofs:proof-simple-commitment
                                                   'utx-send-cmt)
                                       (trans-send-utxs trn))))
             (tspend   (reduce 'ed-add cspends
                               :initial-value (ed-neutral-point)))
             (tsend    (reduce 'ed-add csends
                               :initial-value (ed-neutral-point))))
        (ed-neutral-point-p (ed-add (ed-mul (range-proofs:hpt) gamma)
                                    (ed-sub tspend tsend)))
        ))))

;; ------------------------------------------------------------------
#|
(let* ((k    (pbc:make-key-pair :dave))
       (pkey (pbc:keying-triple-pkey k))
       (skey (pbc:keying-triple-skey k)))
  (multiple-value-bind (utxin info) 
      (make-utx-spend 1000 1 pkey skey)
    (let* ((km   (pbc:make-key-pair :mary))
           (pkeym (pbc:keying-triple-pkey km)))
      (multiple-value-bind (utxo1 secr1)
          (make-utx-send 750 pkeym)
        (multiple-value-bind (utxo2 secr2)
            (make-utx-send 250 pkey)
          (let ((trans (make-transaction `(,utxin) `(,info)
                                         `(,utxo1 ,utxo2)
                                         `(,secr1 ,secr2))))
            (inspect trans)
            (time (validate-transaction trans)) ;; 7.6s MacBook Pro
            ;; (inspect utx)
            ;; (validate-spend-utx utx)
            ))))))
 |#
