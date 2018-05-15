
(in-package :cosi/proofs)

;; ----------------------------------------------------------------------------------

(defclass txin ()
  ((hashlock  :reader   txin-hashlock ;; h = H(C, P)
              :initarg  :hashlock)
   (prf       :reader   txin-prf      ;; Bulletproof on amount - includes C
              :initarg  :prf)
   (pkey      :reader   txin-pkey     ;; P in G_2
              :initarg  :pkey)
   (sig       :accessor txin-sig      ;; Sig(H(T), P) in G_1
              :initarg  :sig)))
  
(defclass cloaked-txin (txin)
  ((encr      :reader  cloaked-txin-encr     ;; Encrypted amount info
              :initarg :encr)
   (cloaked   :reader  txin-cloaked-p
              :allocation :class
              :initform t))
  (:documentation "The 5-tuple that represents the txin side of a simple transaction"))

(defclass uncloaked-txin (txin)
  ((amt       :reader  uncloaked-txin-amt
              :initarg :amt)
   (cloaked   :reader  txin-cloaked-p
              :allocation :class
              :initform nil))
  (:documentation "The 5-tuple that represents an uncloaked txin"))

;; ---------------------------------------------------------------------------------------

(defclass txout ()
  ((hashpkey  :reader  txout-hashpkey ;; hash of recipient's public key
              :initarg :hashpkey)
   (hashlock  :reader  txout-hashlock ;; h = H(C, P) -- this is the UTX ID, and locks TXOUT to recipient
              :initarg :hashlock)
   (prf       :reader  txout-prf      ;; Bulletproof on sent amount - includes C
              :initarg :prf)))
  
(defclass cloaked-txout (txout)
  ((encr      :reader  cloaked-txout-encr     ;; encrypted info for future txining of UTX
              :initarg :encr)
   (cloaked   :reader  txout-cloaked-p
              :allocation :class
              :initform t))
  (:documentation "The 4-tuple that represents a TXOUT"))

(defclass uncloaked-txout (txout)
  ((gamma    :reader  uncloaked-txout-gamma
             :initarg :gamma)
   (amt      :reader  uncloaked-txout-amt
             :initarg :amt)
   (cloaked  :reader  txout-cloaked-p
             :allocation :class
             :initform nil)))

(defclass stake-txout (uncloaked-txout)
  ())

;; --------------------------------------------------------------------------------------------

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

(defmethod hashable-contents ((txin txin))
  (list (txin-hashlock txin)
        (txin-prf txin)
        (txin-pkey txin)))

(defmethod hashable-contents :around ((txin cloaked-txin))
  (list* (cloaked-txin-encr txin)
         (call-next-method)))

(defmethod hashable-contents :around ((txin uncloaked-txin))
  (list* (uncloaked-txin-amt txin)
         (call-next-method)))

;; ---------------------------------------------------------------------

(defmethod pedersen-commitment ((prf range-proofs:range-proof))
  (ed-decompress-pt (range-proofs:proof-simple-commitment prf)))

(defmethod pedersen-commitment ((utx cloaked-txin))
  (pedersen-commitment (txin-prf utx)))

(defmethod pedersen-commitment ((utx uncloaked-txin))
  (ed-decompress-pt (txin-prf utx)))

(defmethod pedersen-commitment ((utx cloaked-txout))
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
       (< amt #.(ash 1 range-proofs:*max-bit-length*))))

(defun need-valid-amt (amt)
  (unless (check-amt amt)
    (error "Amount out of range [0..2^64): ~A" amt)))

;; ---------------------------------------------------------------------

(defun make-cloaked-txin (amt gam pkey skey)
  "Make a TXIN with value proof"
  (need-valid-amt amt)
  (let* ((prf       (range-proofs:make-range-proof amt :gamma gam))
         (hashlock  (make-hashlock prf pkey))
         (encr      (ibe-encrypt amt pkey :spend-amount)))
    (values 
     (make-instance 'cloaked-txin
                    :hashlock  hashlock
                    :prf       prf
                    :pkey      pkey
                    :sig       skey  ;; !!WARNING!!
                    :encr      encr)
     gam)))

(defun make-uncloaked-txin (amt gam pkey skey)
  "Make a signed uncloaked TXIN. Someone must sign for it. When no
pre-existing UTXO, e.g., sum of block fees and confiscated stakes,
only the epoch leader."
  (need-valid-amt amt)
  (let* ((cmt      (make-admin-commit amt gam))
         (hashlock (make-hashlock cmt pkey)))
    (values
     (make-instance 'uncloaked-txin
                    :hashlock  hashlock
                    :prf       (ed-compress-pt cmt)
                    :pkey      pkey
                    :sig       skey ;; !!WARNING!!
                    :amt       amt)
     gam)))

;; -------------------------------------------------------------
    
(defmethod make-cloaked-txout ((amt integer) (pkey pbc:public-key))
  "Make a cloaked TXOUT with value proof"
  (need-valid-amt amt)
  (multiple-value-bind (prf gamma)
      (range-proofs:make-range-proof amt)
    (let ((info  (make-instance 'txout-secrets
                                :pkey  pkey
                                :amt   amt
                                :gamma gamma)))
      (values
       (make-instance 'cloaked-txout
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
  (make-uncloaked-txout amt (pbc:get-g2) :class 'stake-txout))

;; ------------------------------------------------------------------------------

(defclass transaction ()
  ((txins   :reader   trans-txins   ;; txin from current owner
            :initarg  :txins)
   (txouts  :reader   trans-txouts  ;; txout or integer, to new owner or self
            :initarg  :txouts)
   (fee     :reader   trans-fee
            :initarg  :fee
            :initform 0)
   (gamadj  :reader   trans-gamadj  ;; A curve adjustment to get to zero balance.
            :initarg  :gamadj)
   (sig     :accessor trans-signature
            :initarg  :sig)
   ))

;; ---------------------------------------------------------------------

(defmethod hash-trn ((trn transaction))
  (hash:hash/256 (mapcar 'hashable-contents (trans-txins trn))
                 (trans-txouts trn)
                 (trans-fee trn)
                 (trans-gamadj trn)))

(defun do-make-transaction (txins gam-txins txouts txout-secrets fee)
  "TXINS is a list of TXIN structs, TXOUTS is a list of TXOUT structs,
some cloaked, some not.  Add up the txins, subtract the txouts. Result
should be zero value, but some non-zero gamma sum. We make a
correction factor gamadj on curve H for the overall transaction."
  (when (some 'txout-cloaked-p txouts)
    (unless (some 'txin-cloaked-p txins)
      (error "Cannot preserve TXOUT condfidentiality without cloaked TXIN")))
  (let* ((gam-txouts (mapcar 'txout-secr-gamma txout-secrets))
         (gamadj     (with-mod *ed-r*
                       ;; adjustment factor = Sum(gamma_txouts) - Sum(gamma_txinx)
                       ;; so that adding all txin Pedersen commitments,
                       ;; subtracting sum of all txout Pedersen commitents,
                       ;; then adding gamma_adj * Hpt => ECC(0) 
                       (m- (reduce 'm+ gam-txouts)
                           (reduce 'm+ gam-txins))))
         (trn   (make-instance 'transaction
                               :txins  txins
                               :txouts txouts
                               :fee    fee
                               :gamadj gamadj))
         (hash  (hash-trn trn))
         (skeys (mapcar 'txin-sig  txins))
         (sigs  (mapcar (um:curry 'sign-hash hash) skeys)))
    (loop for sig  in sigs
          for txin in txins
          do
          (setf (txin-sig txin) sig))
    (setf (trans-signature trn) (reduce 'add-pts (cdr sigs)
                                        :initial-value (car sigs)))
    trn))

(defun make-transaction (&key ins outs (fee 0))
  "Using this declarative interface instead of the more cumbersome
imperative DO-MAKE-TRANSACTION ensures that the secret keys used
during TXIN formation will be properly disposed of."
  (let ((txins     nil)
        (gam-txins nil)
        (txouts    nil)
        (txout-secrets nil)
        (trn       nil))
    (unwind-protect
        (labels
            ((digest-txin-descr (&key kind amount gamma pkey skey)
               (multiple-value-bind (txin gam)
                   (funcall (ecase kind
                              (:cloaked   'make-cloaked-txin)
                              (:uncloaked 'make-uncloaked-txin))
                            amount gamma pkey skey)
                 (push txin txins)
                 (push gam  gam-txins)))
             
             (digest-txout-descr (&key kind amount pkey)
               (ecase kind
                 (:cloaked  (multiple-value-bind (txout secr)
                                (make-cloaked-txout amount pkey)
                              (push txout txouts)
                              (push secr  txout-secrets)))
                 (:uncloaked (multiple-value-bind (txout secr)
                                 (make-uncloaked-txout amount pkey)
                               (push txout txouts)
                               (push secr  txout-secrets)))
                 (:stake      (multiple-value-bind (txout secr)
                                  (make-stake-txout amount)
                                (push txout txouts)
                                (push secr  txout-secrets)))
                 )))
          (dolist (txin-descr ins)
            (apply #'digest-txin-descr txin-descr))
          (dolist (txout-descr outs)
            (apply #'digest-txout-descr txout-descr))
          (setf trn (do-make-transaction txins gam-txins txouts txout-secrets fee)))
      
      ;; unwind protect...
      (unless trn
        (dolist (txin txins)
          (setf (txin-sig txin) nil))))
    trn))

;; ---------------------------------------------------------------------

(defmethod validate-txin ((utx cloaked-txin) hash)
  (let* ((hl  (make-hashlock (txin-prf utx)
                             (txin-pkey utx))))
    (and (= (int hl)
            (int (txin-hashlock utx)))
         (pbc:check-hash hash
                         (txin-sig utx)
                         (txin-pkey utx))
         (range-proofs:validate-range-proof (txin-prf utx))
         )))

(defmethod validate-txin ((utx uncloaked-txin) hash)
  (let* ((amt  (uncloaked-txin-amt utx))
         (hl   (make-hashlock (pedersen-commitment utx)
                              (txin-pkey utx))))
    (and (check-amt amt)
         (= (int hl)
            (int (txin-hashlock utx)))
         (pbc:check-hash hash
                         (txin-sig utx)
                         (txin-pkey utx)))))

(defmethod validate-txout ((utx cloaked-txout))
  (range-proofs:validate-range-proof (txout-prf utx)))

(defmethod validate-txout ((utx uncloaked-txout))
  (check-amt (uncloaked-txout-amt utx)))

(defmethod validate-transaction ((trn transaction))
  "Validate amounts and zero balance of transaction, not double txining check"
  (with-accessors ((txins   trans-txins)
                   (txouts  trans-txouts)
                   (gamadj  trans-gamadj)
                   (fee     trans-fee)) trn
    (let ((hash  (hash-trn trn)))
      (when (and (every (um:rcurry 'validate-txin hash) txins)
                 (every 'validate-txout txouts))
        (let* ((ctxins  (mapcar 'pedersen-commitment txins))
               (ttxin   (reduce 'ed-add ctxins
                                :initial-value (ed-neutral-point)))
               (ctxouts   (mapcar 'pedersen-commitment txouts))
               (ttxout    (reduce 'ed-add ctxouts
                                  :initial-value (ed-nth-pt fee))))
          ;; check that Sum(txin) = Sum(txout) + Fee
          (ed-pt= (ed-mul (range-proofs:hpt) gamadj)
                  (ed-sub ttxout ttxin)))
        ))))

;; ---------------------------------------------------------------------
;; Recover spend info

(defmethod get-txin-amount ((txin uncloaked-txin) skey)
  (declare (ignore skey))
  (uncloaked-txin-amt txin))

(defmethod get-txin-amount ((txin cloaked-txin) skey)
  (decrypt-txin-info txin skey))



(defmethod decrypt-txin-info  ((txin cloaked-txin) skey)
  (pbc:ibe-decrypt (cloaked-txin-encr txin) skey))

(defmethod find-txin-for-pkey-hash (pkey (trn transaction))
  (find pkey (trans-txins trn)
        :key 'txin-pkey
        :test 'int=))


(defmethod decrypt-txout-info ((txout cloaked-txout) skey)
  (pbc:ibe-decrypt (cloaked-txout-encr txout) skey))

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
  (let ((trans (make-transaction :ins `((:kind :cloaked
                                         :amount 1000
                                         :gamma  1
                                         :pkey   ,pkey
                                         :skey   ,skey))
                                 :outs `((:kind :cloaked
                                          :amount 750
                                          :pkey   ,pkeym)
                                         (:kind :cloaked
                                          :amount 240
                                          :pkey   ,pkey))
                                 :fee 10)))
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
      (let ((trans (make-transaction :ins `((:kind :cloaked
                                             :amount ,(txout-secr-amt minfo)
                                             :gamma  ,(txout-secr-gamma minfo)
                                             :pkey   ,pkeym
                                             :skey   ,skeym))
                                     :outs `((:kind :cloaked
                                              :amount 240
                                              :pkey   ,pkeym)
                                             (:kind :cloaked
                                              :amount 500
                                              :pkey  ,pkey))
                                     :fee 10)))
        (inspect trans)
        
        (print "Validate 2nd transaction")
        (time (assert (validate-transaction trans)))
        ))))

;; ------------------------------------------------------------
;; test uncloaked TXIN/TXOUT

(let* ((k     (pbc:make-key-pair :dave)) ;; genesis keying
       (pkey  (pbc:keying-triple-pkey k))
       (skey  (pbc:keying-triple-skey k))
       
       (km    (pbc:make-key-pair :mary)) ;; Mary keying
       (pkeym (pbc:keying-triple-pkey km))
       (skeym (pbc:keying-triple-skey km)))
  
  (print "Construct Genesis transaction")
  (let ((trans (make-transaction :ins `((:kind :uncloaked
                                         :amount 1000
                                         :gamma  1
                                         :pkey   ,pkey
                                         :skey   ,skey))
                                 :outs `((:kind :uncloaked
                                          :amount 750
                                          :pkey   ,pkeym)
                                         (:kind :uncloaked
                                          :amount 240
                                          :pkey   ,pkey))
                                 :fee 10)))
    (inspect trans)
    
    (print "Validate transaction")
    (time (assert (validate-transaction trans))) ;; 7.6s MacBook Pro
    ;; (inspect utx)
    ;; (validate-txin utx)
    
    (print "Find UTX for Mary")
    (let* ((utxm   (find-txout-for-pkey-hash (hash:hash/256 pkeym) trans))
           (amt    (uncloaked-txout-amt utxm))
           (gamma  (uncloaked-txout-gamma utxm)))
      
      (print "Construct 2nd transaction")
      (let ((trans (make-transaction :ins `((:kind :uncloaked
                                             :amount ,amt
                                             :gamma  ,gamma
                                             :pkey   ,pkeym
                                             :skey   ,skeym))
                                     :outs `((:kind :uncloaked
                                              :amount 240
                                              :pkey   ,pkeym)
                                             (:kind :uncloaked
                                              :amount 500
                                              :pkey  ,pkey))
                                     :fee 10)))
        (inspect trans)
        
        (print "Validate 2nd transaction")
        (time (assert (validate-transaction trans)))
        ))))
|#
