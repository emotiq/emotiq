(in-package :emotiq/txn)

(defparameter *transaction-fee* 10)

;;;; ADDRESS method could ideally be placed in EMOTIQ or EMOTIQ/USER package?
(defmethod address ((pkey pbc:public-key))
  (cosi/proofs:public-key-to-address pkey))
(defmethod address ((account pbc:keying-triple))
  (address (pbc:keying-triple-pkey account)))
(defmethod address ((integers cons))
  ;; not really a list of integers any more -- a list of public keys (tagged items)
  (address (pbc:make-keying-triple (first integers) (second integers))))

(defmethod get-utxos ((account pbc:keying-triple))
  (get-utxos (address account)))
(defmethod get-utxos ((address pbc:address))
  (cosi-simgen:with-current-node cosi-simgen:*my-node*
    (cosi/proofs/newtx:get-utxos-per-account address)))
         

(defun make-spend-transaction (from-account ;; pbc:keying-triple
                               to-address     ;; target address
                               amount
                               &key
                                 (fee *transaction-fee*)
                                 (from-utxos nil)) ;; (list (TXO (TxID INDEX) AMT) ... )
  (let* ((from-skey (pbc:keying-triple-skey from-account))
         (from-pkey (pbc:keying-triple-pkey from-account))
         (from-address (address from-pkey))
         (from-utxos (or from-utxos (get-utxos from-address))))
    (multiple-value-bind (inputs outputs)
        (make-inputs/outputs from-address to-address amount fee from-utxos)
      (let ((transaction (cosi/proofs/newtx::make-newstyle-transaction inputs outputs :spend)))
        (cosi/proofs/newtx:sign-transaction transaction from-skey from-pkey)
        transaction))))


(defun make-inputs/outputs (from-address to-address amount fee candidates
                            &key
                              (get-amount #'third)
                              (get-input-spec #'second))
  (flet ((get-output-spec (address amount)
           (list address amount)))
    (let ((charged-amount (+ amount fee))
          (input-specs nil)
          (output-specs nil))
      (multiple-value-bind (selected-amount txns-to-spend txn-to-split?)
          (select-inputs candidates charged-amount get-amount)
        (when txn-to-split?
          (let* ((amount-to-change (- selected-amount charged-amount))
                 (output-spec (get-output-spec from-address amount-to-change))
                 (input-spec (funcall get-input-spec txn-to-split?)))
            (push input-spec input-specs)
            (push output-spec output-specs)))
        (loop :for txn :in txns-to-spend
           :do (push (funcall get-input-spec txn) input-specs))
        (push (get-output-spec to-address amount) output-specs)
        (values (cosi/proofs/newtx:make-transaction-inputs input-specs)
                (cosi/proofs/newtx:make-transaction-outputs output-specs))))))

;; (TXO (TxID INDEX) AMT)
;; ideally we would use knapsack algorithm, doing it naively for now
(defun select-inputs (candidates amount get-amount)
  (let ((candidates (sort (coerce candidates 'vector) #'< :key get-amount))
        (selected-amount 0)
        (selected-txns nil))
    (flet ((exact ()
             (return-from select-inputs
               (values selected-amount selected-txns nil)))
           (surplus ()
             (destructuring-bind (txn-to-split . rem-txns)
                 selected-txns
               (return-from select-inputs 
                 (values selected-amount rem-txns txn-to-split)))))
      (loop
         :for txn-spec :across candidates
         :as tx-amount = (funcall get-amount txn-spec)
         :do (incf selected-amount tx-amount)
         :do (push txn-spec selected-txns)
         :do (cond ((= selected-amount amount) (exact))
                   ((> selected-amount amount) (surplus))))
      (error "insufficient funds in txns=~A for amount=~A" selected-txns amount))))


;; UTXOS: (list (TXO (TxID INDEX) AMT) ...)
;; (defun select-utxos (utxos amount)
;;   (let ((utxos (sort (coerce utxos 'vector) #'< :key #'third))
;;         (utxos-amount 0)
;;         (selected-utxos nil))
;;     (flet ((utxos-exact ()
;;              (return-from select-utxos
;;                (values utxos-amount selected-utxos nil)))
;;            (utxos-surplus ()
;;              (destructuring-bind (utxo-to-split . rem-utxos)
;;                  selected-utxos
;;                (return-from select-utxos 
;;                  (values utxos-amount rem-utxos utxo-to-split)))))
;;       (loop
;;          :for utxo-spec :across utxos
;;          :as (nil nil tx-amount) = utxo-spec
;;          :do (incf utxos-amount tx-amount)
;;          :do (push utxo-spec selected-utxos)
;;          :do (cond ((= utxos-amount amount) (utxos-exact))
;;                    ((> utxos-amount amount) (utxos-surplus))))
;;       (error "insufficient funds: UTXOs = ~A" selected-utxos))))



;; (defstruct (account 
;;             (:constructor make-account-structure))
;;   name
;;   key-pair     ; optionally nil
;;   public-key
;;   private-key
;;   address)

;; (defun make-account (user-name)
;;   (let* ((key-pair (pbc:make-key-pair user-name))
;;          (public-key (pbc:keying-triple-pkey key-pair))
;;          (private-key (pbc:keying-triple-skey key-pair)))
;;     (make-account-structure
;;      :name user-name 
;;      :key-pair key-pair
;;      :public-key public-key
;;      :private-key private-key
;;      :address (cosi/proofs:public-key-to-address public-key))))

;; (defun make-genesis-account ()
;;   ;; Currently, the configuration is very simple, as follows:
;;   ;;
;;   ;;   :address-for-coin - bignum representing public key to which to
;;   ;;     send initial/total coinbase infusion. This is simple the same
;;   ;;     as the public key of the node, same as :public.
;;   ;;   :public - bignum representing public key of the node
;;   ;;   :private - bignum representing private key of the node
;;   (let ((config-settings (emotiq/config:setting/read)))
;;     (flet ((config-get (name) (cdr (assoc name config-settings))))
;;       (let* ((address-for-coin (config-get :address-for-coins))
;;              (public-key (config-get :public))
;;              (private-key (config-get :private))
;;              (user-name :genesis))
;;         (unless (equal address-for-coin public-key)
;;           (break "not"))                ; compares 2 bignums

;;         (setq public-key
;;               (make-instance 'pbc:public-key :val public-key))
;;         (setq address-for-coin
;;               (cosi/proofs:public-key-to-address public-key))
;;         (setq private-key
;;               (make-instance 'pbc:secret-key :val private-key))          
          
;;         (make-account-structure
;;          :name user-name
;;          :key-pair nil
;;          :public-key public-key
;;          :private-key private-key
;;          :address (cosi/proofs:public-key-to-address public-key))))))





;; (defvar *all-accounts* '())

;; (defun get-account (user-name)
;;   (cdr (assoc user-name *all-accounts*)))

;; (defun set-account (user-name account)
;;   (let ((a (assoc user-name *all-accounts*)))
;;     (if a
;;         (setf (cdr a) account)
;;         (progn 
;;           (push (cons user-name account) *all-accounts*)
;;           account))))

;; (defsetf get-account set-account)





;; (defparameter *white-space*
;;   '(#\space #\tab #\return #\linefeed))

;; (defvar *keyword-package*
;;   (find-package "KEYWORD"))

;; (defun normalize-user-designation (user-designation)
;;   (intern
;;    (string-upcase
;;     (string-trim *white-space* (string user-designation)))
;;    *keyword-package*))

;; ;; Consider:
;; ;;   - internal sequence of whitespace, hyphen, or dash => hyphen
;; ;;   - require initial alpha or digit
;; ;;   - require one or more alpha
;; ;;   - reject some, discard some, convert some





;; (defun add-account (user-designation)
;;   "Add-account for user-designation, a string or symbol in any
;;    package. Return its corresponding account object."
;;   (let* ((user-name (normalize-user-designation user-designation))
;;          (account
;;            (case user-name
;;              (:genesis (make-genesis-account))
;;              (otherwise (make-account user-name)))))
;;     (setf (get-account user-name) account)
;;     account))

;; (defun get-account-utxos (user-designation)
;;   (let* ((user-name (normalize-user-designation user-designation))
;;          (account? (get-account user-name))
;;          (address? (and account? (account-address account?))))
;;     (and address?
;;          (cosi/proofs/newtx:get-utxos-per-account address?))))

;; (defun make-tx-input-spec (txid index)
;;   `(,txid ,index))

;; (defun make-tx-output-spec (address amount)
;;   `(,address ,amount))

;; (defun make-tx-input-specs (&rest input-specs)
;;   (loop for (txid index) in input-specs
;;         collect (make-tx-input-spec txid index)))

;; (defun make-tx-output-specs (&rest output-specs)
;;   (loop for (address amount) in output-specs
;;         collect (make-tx-output-spec address amount)))



  

  
;; (defun make-transaction-per-txo-info 
;;     (from-user-designation txo-info to-user-designation to-amount fee)
;;   (destructuring-bind (txo (txid index) amount)
;;       txo-info
;;     (declare (ignore txo))
;;     (let* ((from-account
;;              (get-account
;;               (normalize-user-designation from-user-designation)))
;;            (to-account
;;              (get-account 
;;               (normalize-user-designation to-user-designation)))
;;            change-amount
;;            transaction)
;;       (when (null from-account)
;;         (error "The FROM: user's account designation ~s is unrecognized." 
;;                from-user-designation))
;;       (when (null to-account)
;;         (error "The TO: user's account designation ~s is unrecognized." 
;;                to-user-designation))
;;       (unless (<= (+ to-amount fee) amount)
;;         (error "Not enough coin"))
;;       (setq change-amount          ; hmmm, currently allow zero change
;;             (- amount (+ to-amount fee)))
;;       (setq transaction
;;             (cosi/proofs/newtx:make-transaction
;;              (cosi/proofs/newtx:make-transaction-inputs
;;               (make-tx-input-specs `(,txid ,index))
;;               :transaction-type :spend)
;;              (cosi/proofs/newtx:make-transaction-outputs
;;               (if (zerop change-amount)
;;                   (make-tx-output-specs
;;                    `(,(account-address to-account) ,to-amount))
;;                   (make-tx-output-specs
;;                    `(,(account-address to-account) ,to-amount)
;;                    `(,(account-address from-account) ,change-amount)))
;;               :transaction-type :spend)
;;              :spend))
;;       (cosi/proofs/newtx:sign-transaction 
;;        transaction
;;        (account-private-key from-account)
;;        (account-public-key from-account))
;;       transaction)))

  



;; (defun init-accounts ()
;;   (add-account :genesis)

;;   (add-account :user-1)
;;   (add-account :user-2)
;;   (add-account :user-3)

;;   ;; some of "Cast of Characters" from Alice and Bob wiki
;;   (add-account :alice)
;;   (add-account :bob)
;;   (add-account :carol)
;;   (add-account :dave)
;;   (add-account :eve)
;;   (add-account :faythe)
;;   (add-account :grace)
;;   (add-account :mallory)
;;   (add-account :trent))


;; (defun demo-genesis-make-tx ()
;;   "Make a Spend from the first Genesis UTXO to User-1. Send 50,000 of coin,
;;    with 300 as fee, returning rest back to self (Genesis) as change."
;;   (let ((first-utxo (first (get-account-utxos :genesis))))
;;     (make-transaction-per-txo-info :genesis first-utxo :user-1 50000 300)))

;; (defun demo-genesis-validate-tx ()
;;   (let ((tx (demo-genesis-make-tx)))
;;     (cosi/proofs/newtx:validate-transaction tx)
;;     tx))

;; ;; Demo script:
;; ;; 
;; ;;   (ql:quickload :emotiq/startup)
;; ;;   (emotiq:main)
;; ;;   ;; wait a minute...
;; ;;   (setq cosi-simgen:*current-node* cosi-simgen:*my-node*) ; must return non-nil
;; ;;   (cosi/proofs/newtx:dump-txs :blockchain t :mempool t)
;; ;;   (init-accounts)
;; ;;   (demo-genesis-validate-tx)
;; ;;   (cosi/proofs/newtx:dump-txs :blockchain t :mempool t)



;; (defun demo-genesis-pay-anyone (to-user-designation amount fee)
;;   (let ((utxos (get-account-utxos :genesis)))
;;     (loop with needed-amount = (+ amount fee)
;;           for utxo in utxos
;;           as (nil nil unspent-amount) = utxo
;;           when (>= unspent-amount needed-amount)
;;             return
;;             (let ((tx (make-transaction-per-txo-info
;;                        :genesis utxo to-user-designation
;;                        amount fee)))
;;               (cosi/proofs/newtx:validate-transaction tx)
;;               tx)
;;           finally
;;              (format t "~%Sorry, could not find a UTXO with enough funds.~%")
;;              (return nil))))

;; Demo script 2:
;; 
;;   (ql:quickload :emotiq/startup)
;;   (emotiq:main)
;;   ;; wait a minute...
;;   (setq cosi-simgen:*current-node* cosi-simgen:*my-node*) ; must return non-nil
;;   (cosi/proofs/newtx:dump-txs :blockchain t :mempool t)
;;   (init-accounts)
;;   (demo-genesis-pay-anyone :trent 50113 313)
;;   (cosi/proofs/newtx:get-balance (account-address (get-account :genesis)))  ; may show 0
;;   (cosi/proofs/newtx:dump-txs :blockchain t :mempool t)
;;
;; Note: if the transaction sits in the mempool, does not get into a
;; block, get-balance will not consider it. Therefore, balance shows
;; as 0 even after successfully going through.  Similarly, trying to
;; spend the output to change address pending in the mempool will not
;; work; will be rejected as a double spend.
