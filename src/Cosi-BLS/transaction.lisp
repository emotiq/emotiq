
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
  ((hashpkey :reader  utx-send-hashpkey
             :initarg :hashpkey)
   (hashlock :reader  utx-send-hashlock
             :initarg :hashlock)
   (amt      :reader  utx-uncloaked-send-amt
             :initarg :amt)
   (gamma    :reader  utx-uncloaked-send-gamma
             :initarg :gamma)))

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

(defmethod pedersen-commitment ((prf range-proofs:range-proof))
  (ed-decompress-pt (range-proofs:proof-simple-commitment prf)))

(defmethod pedersen-commitment ((utx utx-spend))
  (pedersen-commitment (utx-spend-cmt utx)))

(defmethod pedersen-commitment ((utx utx-send))
  (pedersen-commitment (utx-send-cmt utx)))

(defmethod pedersen-commitment ((utx utx-uncloaked-send))
  (ed-nth-pt (utx-uncloaked-send-amt utx)))

(defmethod make-hashlock ((prf range-proofs:range-proof) (pkey pbc:public-key))
  "We are often given a long value proof, and we need the commitment
to the uncloaked value"
  (hash:hash/256 (pedersen-commitment prf) pkey))

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
                      :encr      (pbc:ibe-encrypt info pkey :spend-info))
       info))))

(defmethod make-utx-send ((amt integer) (pkey null))
  "Make an uncloaked send with raw value showing"
  (let* ((pkey     (pbc:get-g2))
         (gamma    (random-between 1 *ed-r*))
         (cmt      (range-proofs:simple-commit (range-proofs:hpt) gamma amt))
         (hashlock (hash:hash/256 cmt pkey)))
    (values
     (make-instance 'utx-uncloaked-send
                    :hashlock hashlock
                    :hashpkey (hash:hash/256 pkey)
                    :amt      amt)
     (make-instance 'utx-send-secrets
                    :pkey pkey
                    :amt  amt
                    :gam  gamma))
    ))

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
                       ;; adjustment factor = Sum(gamma_sends) - Sum(gamma_spends)
                       ;; so that adding all spend Pedersen commitments,
                       ;; subtracting sum of all send Pedersen commitents,
                       ;; then adding gamma_adj * Hpt => ECC(0) 
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
      (let* ((cspends  (mapcar 'pedersen-commitment (trans-spend-utxs trn)))
             (tspend   (reduce 'ed-add cspends
                               :initial-value (ed-neutral-point)))
             (csends   (mapcar 'pedersen-commitment (trans-send-utxs  trn)))
             (tsend    (reduce 'ed-add csends
                               :initial-value (ed-neutral-point))))
        ;; check that spend = send
        (ed-neutral-point-p (ed-add (ed-mul (range-proofs:hpt) gamma)
                                    (ed-sub tspend tsend)))
        ))))

(defmethod find-utx-for-pkey-hash (pkey-hash (trn transaction))
  (find pkey-hash (trans-send-utxs trn)
        :key  'utx-send-hashpkey
        :test (lambda (k1 k2)
                (= (int k1) (int k2)))))

;; ------------------------------------------------------------------
#|
 ;; Test it out by creating a genesis transaction for 1000 tokens,
 ;; spend 750 on Mary, return 250 to genesis. Validate transaction.
 ;;
 ;; Now Mary searches for her UTX, constructs a 2nd transaction
 ;; sending 250 back to herself, and 500 back to genesis. Validate transaction.
 ;;
(let* ((k     (pbc:make-key-pair :dave)) ;; genesis keying
       (pkey  (pbc:keying-triple-pkey k))
       (skey  (pbc:keying-triple-skey k))
       
       (km    (pbc:make-key-pair :mary)) ;; Mary keying
       (pkeym (pbc:keying-triple-pkey km))
       (skeym (pbc:keying-triple-skey km)))
  
  (print "Construct Genesis transaction")
  (multiple-value-bind (utxin info)  ;; spend side
      (make-utx-spend 1000 1 pkey skey)
    
    (multiple-value-bind (utxo1 secr1) ;; sends
        (make-utx-send 750 pkeym)
      (multiple-value-bind (utxo2 secr2)
          (make-utx-send 250 pkey)
        
        (let ((trans (make-transaction `(,utxin) `(,info)
                                       `(,utxo1 ,utxo2)
                                       `(,secr1 ,secr2))))
          (inspect trans)
          
          (print "Validate transaction")
          (time (assert (validate-transaction trans))) ;; 7.6s MacBook Pro
          ;; (inspect utx)
          ;; (validate-spend-utx utx)
          
          (print "Find UTX for Mary")
          (let* ((utxm   (find-utx-for-pkey-hash (hash:hash/256 pkeym) trans))
                 (minfo  (decrypt-spend-info (utx-send-encr utxm) skeym)))
            (inspect minfo)
            
            (print "Construct 2nd transaction")
            (multiple-value-bind (utxin info)  ;; spend side
                (make-utx-spend (utx-send-secr-amt minfo)
                                (utx-send-secr-gam minfo)
                                pkeym skeym)
              
              (multiple-value-bind (utxo1 secr1) ;; sends
                  (make-utx-send 250 pkeym)
                (multiple-value-bind (utxo2 secr2)
                    (make-utx-send 500 pkey)
                  
                  (let ((trans (make-transaction `(,utxin) `(,info)
                                                 `(,utxo1 ,utxo2)
                                                 `(,secr1 ,secr2))))
                    (inspect trans)

                    (print "Validate 2nd transaction")
                    (time (assert (validate-transaction trans)))
                    )))))
          )))))
 |#
