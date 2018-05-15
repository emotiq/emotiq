
(in-package :cosi/proofs)

;; ----------------------------------------------------------------------------------
  
(defclass txin ()
  ((hashlock  :reader  txin-hashlock ;; h = H(C, P)
              :initarg :hashlock)
   (prf       :reader  txin-prf      ;; Bulletproof on amount - includes C
              :initarg :prf)
   (pkey      :reader  txin-pkey     ;; P in G_2
              :initarg :pkey)
   (sig       :reader  txin-sig      ;; Sig(h, P) in G_1
              :initarg :sig)
   (encr      :reader  txin-encr     ;; Encrypted amount info
              :initarg :encr)
   (cloaked   :reader  txin-cloaked-p
              :allocation :class
              :initform t))
  (:documentation "The 4-tuple that represents the txin side of a simple transaction"))

(defclass uncloaked-txin ()
  ((hashlock  :reader  txin-hashlock ;; ID of this UTX
              :initarg :hashlock)
   (prf       :reader  txin-prf      ;; the Pedersen commitment for this UTX
              :initarg :prf)
   (pkey      :reader  txin-pkey
              :initarg :pkey)
   (sig       :reader  txin-sig
              :initarg :sig)
   (amt       :reader  txin-amt
              :initarg :amt)
   (cloaked   :reader  txin-cloaked-p
              :allocation :class
              :initform nil))
  (:documentation "The 4-tuple that represents an uncloaked txin"))

(defclass txout ()
  ((hashpkey  :reader  txout-hashpkey ;; hash of recipient's public key
              :initarg :hashpkey)
   (hashlock  :reader  txout-hashlock ;; h = H(C, P) -- this is the UTX ID, and locks TXOUT to recipient
              :initarg :hashlock)
   (prf       :reader  txout-prf      ;; Bulletproof on sent amount - includes C
              :initarg :prf)
   (encr      :reader  txout-encr     ;; encrypted info for future txining of UTX
              :initarg :encr)
   (cloaked   :reader  txout-cloaked-p
              :allocation :class
              :initform t))
  (:documentation "The 4-tuple that represents a TXOUT"))

(defclass uncloaked-txout ()
  ((hashpkey :reader  txout-hashpkey
             :initarg :hashpkey)
   (hashlock :reader  txout-hashlock  ;; = Hash(C, P) - locks this TXOUT to recipient
             :initarg :hashlock)
   (prf      :reader  txout-prf       ;; Pedersen commitment for this UTX
             :initarg :prf)
   (gamma    :reader  uncloaked-txout-gamma
             :initarg :gamma)
   (amt      :reader  uncloaked-txout-amt
             :initarg :amt)
   (cloaked  :reader  txout-cloaked-p
             :allocation :class
             :initform nil)))

(defclass stake-txout (uncloaked-txout)
  ())

(defclass txout-secrets ()
  ((pkey      :reader  txout-secr-pkey ;; pkey used for this receipt, hash to help locate UTX
              :initarg :pkey)
   (amt       :reader  txout-secr-amt  ;; form commitment C = gam*A + amt*B to form
              :initarg :amt)              ;;  the hashlock h = Hash(C, P) = UTX ID
   (gamma     :reader  txout-secr-gamma  ;; use this hiding factor when you re-make the commitment
              :initarg :gamma))
  (:documentation "This is the stuff needed by recipient to be able to reconstruct a txin proof.
It should be stored in the wallet"))

;; ---------------------------------------------------------------------

(defmethod pedersen-commitment ((prf range-proofs:range-proof))
  (ed-decompress-pt (range-proofs:proof-simple-commitment prf)))

(defmethod pedersen-commitment ((utx txin))
  (pedersen-commitment (txin-prf utx)))

(defmethod pedersen-commitment ((utx uncloaked-txin))
  (ed-decompress-pt (txin-prf utx)))

(defmethod pedersen-commitment ((utx txout))
  (pedersen-commitment (txout-prf utx)))

(defmethod pedersen-commitment ((utx uncloaked-txout))
  (ed-decompress-pt (txout-prf utx)))

(defmethod make-hashlock ((prf range-proofs:range-proof) (pkey pbc:public-key))
  "We are often given a long Bulletproof, and we need the to hash on
the simple commitment to the uncloaked value"
  (hash:hash/256 (pedersen-commitment prf) pkey))

(defmethod make-hashlock (arg (pkey pbc:public-key))
  (hash:hash/256 arg pkey))

(defun make-admin-commit (amt &optional (gamma (random-between 1 *ed-r*)))
  (values
   (range-proofs:simple-commit (range-proofs:hpt) gamma amt)
   gamma))

(defun check-amt (amt)
  (and (not (minusp amt))
       (< amt #.(ash 1 64))))

(defun need-valid-amt (amt)
  (unless (check-amt amt)
    (error "Amount out of range [0..2^64): ~A" amt)))

;; ---------------------------------------------------------------------

(defun make-txin (amt gam pkey skey)
  "Make a TXIN with value proof"
  (need-valid-amt amt)
  (let* ((prf       (range-proofs:make-range-proof amt :gamma gam))
         (hashlock  (make-hashlock prf pkey))
         (sig       (pbc:sign-hash hashlock skey))
         (encr      (ibe-encrypt amt pkey :spend-amount)))
    (values 
     (make-instance 'txin
                    :hashlock  hashlock
                    :prf       prf
                    :pkey      pkey
                    :sig       sig
                    :encr      encr)
     gam)))

(defun make-uncloaked-txin (amt gam pkey skey)
  "Make a signed uncloaked TXIN. Someone must sign for it. When no
pre-existing UTXO, e.g., sum of block fees and confiscated stakes,
only the epoch leader."
  (need-valid-amt amt)
  (let* ((cmt      (make-admin-commit amt gam))
         (hashlock (make-hashlock cmt pkey))
         (sig      (pbc:sign-hash hashlock skey)))
    (values
     (make-instance 'uncloaked-txin
                    :hashlock  hashlock
                    :prf       (ed-compress-pt cmt)
                    :pkey      pkey
                    :sig       sig
                    :amt       amt)
     gam)))

;; -------------------------------------------------------------
    
(defmethod make-txout ((amt integer) (pkey pbc:public-key))
  "Make a cloaked TXOUT with value proof"
  (need-valid-amt amt)
  (multiple-value-bind (prf gamma)
      (range-proofs:make-range-proof amt)
    (let ((info  (make-instance 'txout-secrets
                                :pkey  pkey
                                :amt   amt
                                :gamma gamma)))
      (values
       (make-instance 'txout
                      :hashpkey  (hash:hash/256 pkey)     ;; for recipient to locate tokens
                      :hashlock  (make-hashlock prf pkey) ;; the UTX ID
                      :prf       prf                      ;; value proof
                      :encr      (pbc:ibe-encrypt info pkey :spend-amount))
       info))))

(defmethod make-uncloaked-txout ((amt integer) (pkey pbc:public-key) &key (class 'uncloaked-txout))
  "Make an uncloaked TXOUT with raw value showing"
  (need-valid-amt amt)
  (multiple-value-bind (cmt gamma) (make-admin-commit amt)
    (let ((info  (make-instance 'txout-secrets
                                :pkey  pkey
                                :amt   amt
                                :gamma gamma)))
      (values
       (make-instance class
                      :hashpkey (hash:hash/256 pkey)
                      :hashlock (make-hashlock cmt pkey)
                      :prf      (ed-compress-pt cmt)
                      :gamma    gamma
                      :amt      amt)
       info))))

(defmethod make-stake-txout ((amt integer))
  "Make an administrative uncloaked TXOUT with raw value showing.
Typically used for stakes. Public key is G2, Secret key is 1."
  (make-uncloaked-txout amt (pbc:get-g2) 'stake-txout))

;; ------------------------------------------------------------------------------

(defclass transaction ()
  ((txins   :reader  trans-txins   ;; txin from current owner
            :initarg :txins)
   (txouts  :reader  trans-txouts  ;; txout or integer, to new owner or self
            :initarg :txouts)
   (fee     :reader  trans-fee
            :initarg :fee
            :initform 0)
   (gamadj  :reader  trans-gamadj  ;; A curve adjustment to get to zero balance.
            :initarg :gamadj)
   (sig     :reader  trans-signature
            :initarg :sig)
   ))

;; ---------------------------------------------------------------------

(defun make-transaction (txins gam-txins txouts txout-secrets
                               &key (fee 0) skey)
  "TXINS is a list of TXIN structs, TXOUTS is a list of TXOUT structs,
some cloaked, some not.  Add up the txins, subtract the txouts. Result
should be zero value, but some non-zero gamma sum. We make a
correction factor gamadj on curve H for the overall transaction."
  (when (some 'txout-cloaked-p txouts)
    (unless (some 'txin-cloaked-p txins)
      (error "Cannot preserve TXOUT condfidentiality without cloaked TXIN")))
  (let* ((hash  (hash/256 txins txouts))
         (sig   (sign-hash hash skey))
         (gam-txouts (mapcar 'txout-secr-gamma txout-secrets))
         (gamadj     (with-mod *ed-r*
                       ;; adjustment factor = Sum(gamma_txouts) - Sum(gamma_txinx)
                       ;; so that adding all txin Pedersen commitments,
                       ;; subtracting sum of all txout Pedersen commitents,
                       ;; then adding gamma_adj * Hpt => ECC(0) 
                       (m- (reduce 'm+ gam-txouts)
                           (reduce 'm+ gam-txins)))))
    (make-instance 'transaction
                   :txins   txins
                   :txouts  txouts
                   :fee     fee
                   :gamadj  gamadj
                   :sig     sig)))

;; ---------------------------------------------------------------------

(defmethod validate-txin ((utx txin))
  (let* ((hl  (make-hashlock (txin-prf utx)
                             (txin-pkey utx))))
    (and (= (int hl)
            (int (txin-hashlock utx)))
         (pbc:check-hash hl
                         (txin-sig utx)
                         (txin-pkey utx))
         (range-proofs:validate-range-proof (txin-prf utx))
         )))

(defmethod validate-txin ((utx uncloaked-txin))
  (let* ((amt  (txin-amt utx))
         (hl   (make-hashlock (pedersen-commitment utx)
                              (txin-pkey utx))))
    (and (check-amt amt)
         (= (int hl)
            (int (txin-hashlock utx)))
         (pbc:check-hash hl
                         (txin-sig utx)
                         (txin-pkey utx)))))

(defmethod validate-txout ((utx txout))
  (range-proofs:validate-range-proof (txout-prf utx)))

(defmethod validate-txout ((utx uncloaked-txout))
  (check-amt (uncloaked-txout-amt utx)))

(defmethod validate-transaction ((trn transaction))
  "Validate amounts and zero balance of transaction, not double txining check"
  (with-accessors ((txins   trans-txins)
                   (txouts  trans-txouts)
                   (gamadj  trans-gamadj)
                   (sig     trans-signature)) trn
    (let ((pkey  (txin-pkey (first txins)))
          (hash  (hash/256 txins txouts)))
      (when (and (check-hash hash sig pkey)
                 (every 'validate-txin txins)
                 (every 'validate-txout txouts))
        (let* ((ctxins  (mapcar 'pedersen-commitment (trans-txins trn)))
               (ttxin   (reduce 'ed-add ctxins
                                :initial-value (ed-neutral-point)))
               (ctxouts   (mapcar 'pedersen-commitment (trans-txouts trn)))
               (ttxout    (reduce 'ed-add ctxouts
                                  :initial-value (ed-nth-pt (trans-fee trn)))))
          ;; check that Sum(txin) = Sum(txout) + Fee
          (ed-pt= (ed-mul (range-proofs:hpt) gamadj)
                  (ed-sub ttxout ttxin)))
        ))))

;; ---------------------------------------------------------------------
;; Recover spend info

(defmethod get-txin-amount ((txin uncloaked-txin) skey)
  (declare (ignore skey))
  (txin-amt txin))

(defmethod get-txin-amount ((txin txin) skey)
  (decrypt-txin-info txin skey))



(defmethod decrypt-txin-info  ((txin txin) skey)
  (pbc:ibe-decrypt (txin-encr txin) skey))

(defmethod find-txin-for-pkey-hash (pkey (trn transaction))
  (find pkey (trans-txins trn)
        :key 'txin-pkey
        :test 'int=))


(defun decrypt-txout-info (txout skey)
  (pbc:ibe-decrypt (txout-encr txout) skey))

(defmethod find-txout-for-pkey-hash (pkey-hash (trn transaction))
  (find pkey-hash (trans-txouts trn)
        :key  'txout-hashpkey
        :test 'int=))

;; ------------------------------------------------------------------
#|
 ;; Test it out by creating a genesis transaction for 1000 tokens,
 ;; spend 750 on Mary, return 240 to genesis, fees 10. Validate transaction.
 ;; 
 ;; Now Mary searches for her UTX, constructs a 2nd transaction
 ;; sending 240 back to herself, fees 10, and 500 back to genesis. Validate transaction.
 ;;
(let* ((k     (pbc:make-key-pair :dave)) ;; genesis keying
       (pkey  (pbc:keying-triple-pkey k))
       (skey  (pbc:keying-triple-skey k))
       
       (km    (pbc:make-key-pair :mary)) ;; Mary keying
       (pkeym (pbc:keying-triple-pkey km))
       (skeym (pbc:keying-triple-skey km)))
  
  (print "Construct Genesis transaction")
  (multiple-value-bind (utxin info)  ;; spend side
      (make-txin 1000 1 pkey skey)
    
    (multiple-value-bind (utxo1 secr1) ;; sends
        (make-txout 750 pkeym)
      (multiple-value-bind (utxo2 secr2)
          (make-txout 240 pkey)
        
        (let ((trans (make-transaction `(,utxin) `(,info)
                                       `(,utxo1 ,utxo2)
                                       `(,secr1 ,secr2)
                                       :fee 10
                                       :skey skey)))
          (inspect trans)
          
          (print "Validate transaction")
          (time (assert (validate-transaction trans))) ;; 7.6s MacBook Pro
          ;; (inspect utx)
          ;; (validate-txin utx)
          
          (print "Find UTX for Mary")
          (let* ((utxm   (find-txout-for-pkey-hash (hash:hash/256 pkeym) trans))
                 (minfo  (decrypt-txout-info utxm skeym)))
            (inspect minfo)
            
            (print "Construct 2nd transaction")
            (multiple-value-bind (utxin info)  ;; spend side
                (make-txin (txout-secr-amt minfo)
                                (txout-secr-gamma minfo)
                                pkeym skeym)
              
              (multiple-value-bind (utxo1 secr1) ;; sends
                  (make-txout 240 pkeym)
                (multiple-value-bind (utxo2 secr2)
                    (make-txout 500 pkey)
                  
                  (let ((trans (make-transaction `(,utxin) `(,info)
                                                 `(,utxo1 ,utxo2)
                                                 `(,secr1 ,secr2)
                                                 :fee 10
                                                 :skey skeym)))
                    (inspect trans)

                    (print "Validate 2nd transaction")
                    (time (assert (validate-transaction trans)))
                    )))))
          )))))
 |#
