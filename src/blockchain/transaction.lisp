(in-package :emotiq/transaction)

;;;; Coin and Token Units

;;; NB: while the Emotiq Whitepaper talks of EMTQ tokens and EMTQ child tokens,
;;; this is probably simpler than what's there and probably does not match the
;;; level of sophistication implied there.

;;; At a later point we might switch to the term "token", but for now we're
;;; using coin", as follows.  

;;; A `coin unit' or more simply `unit' or `coin' (and plural versions with
;;; `units' or `coins') all refer the fully valued coin in the cyrpto currency
;;; here implemented.

;;; A `coin subunit' or `subunit' (plural `subunits') all refer to the smallest
;;; fraction of a coin unit.

;;; A `coin amount' or `amount' is a number of coin subunits.

;;; When expressing a number of units messages with users, it is fine to use
;;; floats or ratios, but as a coding convention internally we use integer math
;;; only, via the following constants.

(um:defconstant+ +initial-coin-units+ #.(expt 10 9)) ; 1,000,000,000
(um:defconstant+ +subunits-per-coin+ #.(expt 10 8)) ; 100,000,000 (same as Satoshi)
(um:defconstant+ +initial-total-coin-amount+ (* +initial-coin-units+ +subunits-per-coin+)
  "Total number of subunits of coin to be initially allocated in the genesis block.")

(um:defconstant+ +minimum-spend-amount+ 0) ; ---!!! review!!
(um:defconstant+ +maximum-spend-amount+ +initial-total-coin-amount+)

(deftype legal-amount-range () `(integer ,+minimum-spend-amount+ ,+maximum-spend-amount+))

(defun in-legal-money-range-p (amount)
  (typep amount 'legal-amount-range))

(defun in-legal-stake-range-p (amount)
  (in-legal-money-range-p amount))

;; ---!!! Above forms need review!!  -mhd, 7/5/18

;;; The next two values to initialize the id and index of the input for a coinbase transaction
;;; are settings are imitative of Bitcoin, which similarly has all 0 bits for Tx ID, all 1 bits
;;; for Tx index. These slots do not serve their normal function, so these values are arbitrary.
;;; These two ARE part of the hash of the transaction.

(um:defconstant+ +coinbase-input-id+ (make-string 64 :initial-element #\0))
(um:defconstant+ +coinbase-input-index+ -1)


(defun coinbase-input-p (transaction-input)
  (and (id= (input-id transaction-input) +coinbase-input-id+)
       (eql (input-index transaction-input) +coinbase-input-index+)))

(defun make-coinbase-input ()
  (make-instance 'transaction-input
                 :id +coinbase-input-id+
                 :index +coinbase-input-index+
                 :unlock-script nil ;; not going to be used as for a normal transaction
                 :%public-key nil   ;; The rest are NOT part of the 
                 :%signature nil))  ;; hash of the transaction.

(defun make-coinbase-output (address amount)
  (make-instance 'transaction-output
                 :address address
                 :amount amount
                 :lock-script (txn/script:get-lock-script 'txn/script::script-pub-key))) ;; needs work


(defun make-inputs/outputs (from-address to-address amount fee candidates
                            &key
                              (get-amount #'third)
                              (get-input-spec #'second))
  (flet ((get-output-spec (address amount)
           (list address amount)))
    (let ((charged-amount (+ amount fee))
          (input-specs nil)
          (output-specs nil))
      (multiple-value-bind (selected-amount txs-to-spend tx-to-split?)
          (select-inputs candidates charged-amount get-amount)
        (when tx-to-split?
          (let* ((amount-to-change (- selected-amount charged-amount))
                 (output-spec (get-output-spec from-address amount-to-change))
                 (input-spec (funcall get-input-spec tx-to-split?)))
            (push input-spec input-specs)
            (push output-spec output-specs)))
        (loop :for tx :in txs-to-spend
           :do (push (funcall get-input-spec tx) input-specs))
        (push (get-output-spec to-address amount) output-specs)
        (values (make-inputs input-specs)
                (make-outputs output-specs))))))

;; (TXO (TxID INDEX) AMT)
;; ideally we would use knapsack algorithm, doing it naively for now
(defun select-inputs (candidates amount get-amount)
  (let ((candidates (sort (coerce candidates 'vector) #'< :key get-amount))
        (selected-amount 0)
        (selected-txs nil))
    (flet ((exact ()
             (return-from select-inputs
               (values selected-amount selected-txs nil)))
           (surplus ()
             (destructuring-bind (tx-to-split . rem-txs)
                 selected-txs
               (return-from select-inputs 
                 (values selected-amount rem-txs tx-to-split)))))
      (loop
         :for tx-spec :across candidates
         :as tx-amount = (funcall get-amount tx-spec)
         :do (incf selected-amount tx-amount)
         :do (push tx-spec selected-txs)
         :do (cond ((= selected-amount amount) (exact))
                   ((> selected-amount amount) (surplus))))
      (error "insufficient funds ~A" selected-txs))))



(defun make-inputs (input-specs &key (transaction-type :spend))
  "Make transaction inputs for TRANSACTION-TYPE, which defaults to :SPEND."
  (let ((unlock-script (txn/script:get-unlock-script-for-type transaction-type)))
    (loop :for (id index) :in input-specs
       :collect (make-instance 'transaction-input
                               :id id
                               :index index
                               :unlock-script unlock-script))))

(defun make-outputs (output-specs &key (transaction-type :spend))
  (let ((lock-script (txn/script:get-lock-script-for-type transaction-type)))
    (loop :for (address amount) :in output-specs
       :collect (make-instance 'transaction-output
                               :address address
                               :amount amount
                               :lock-script lock-script))))
  

(defun check-skeys-for-inputs (secret-keys tx-inputs)
  (unless (= (length secret-keys) (length tx-inputs))
    (emotiq:em-warn  "Unequal-length secret-keys/tx-input hashes ~a length = ~d vs. ~a length = ~d. Programming error likely!"
                     secret-keys
                     (length secret-keys)
                     tx-inputs
                     (length tx-inputs))))

(defun check-pkeys-for-inputs (public-keys tx-inputs)
  (unless (= (length public-keys) (length tx-inputs))
    (emotiq:em-warn "Unequal-length public-keys/tx-input hashes ~a length = ~d vs. ~a length = ~d. Programming error likely!"
                    public-keys
                    (length public-keys)
                    tx-inputs
                    (length tx-inputs))))


(defmethod sign! ((transaction transaction) skeys pkeys)
  "Sign transactions with secret and public key or keys SKEYS and PKEYS,
   respectively. SKEYS/PKEYS should be supplied as either a single atomic key or
   a singleton list in the case of a single input or as a list of two or more
   keys in the case of two or more inputs. Normally, all transactions should be
   signed, and only coinbase transactions are not signed.  Each resulting
   signature is stored in the %signature slot of each input, and note also
   that the signature stores and makes accessible its corresponding public key."
  (with-slots (inputs) transaction
    (let ((message (signature transaction))
          (secret-keys (if (and skeys (atom skeys)) (list skeys) skeys)))
      (check-skeys-for-inputs secret-keys inputs)
      (let ((public-keys (if (and pkeys (atom pkeys)) (list pkeys) pkeys)))
        (check-pkeys-for-inputs public-keys inputs)
        (loop
           :for input :in inputs
           :as secret-key :in secret-keys
           :as public-key :in public-keys
           :as signature = (pbc:sign-hash (hash:hash/256 message) secret-key)
           :do (psetf (%input-signature input) signature
                      (%input-public-key input) public-key))))))






