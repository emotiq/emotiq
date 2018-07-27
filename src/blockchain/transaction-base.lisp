(in-package :emotiq/transaction)

(defparameter *minimum-transaction-fee* 10)

;;;; Transaction Types

;;; A `transaction' in Emotiq Lisp software is implemented as an instance of class
;;; TRANSACTION. The Lisp type of an instance of this class is TRANSACTION. In
;;; the following discussion, however, the word "type" is used in its generic
;;; meaning, not Lisp/programming-language sense.

;;; Emotiq has six types of transactions:
;;;
;;;   - Spend transaction to transfer tokens, with two subtypes
;;;     - cloaked
;;;     - uncloaked
;;;   - Collect transaction for collecting block fees
;;;   - Stake transaction for sending tokens into PoS escrow
;;;   - Unstake transaction for withdrawing tokens from PoS escrow
;;;   - Slash trasaction to request punishment for a cheating node
;;;   - Disperse transaction to disperse tokens from a cheating node escrow between validators

;;; A transaction instance indentifies its type via its transaction-type slot,
;;; which should be set near the beginning of the life of the transaction
;;; initialized to one of the following valid values, which in turn determine
;;; valid values for other slots of a transaction.  A `transaction type' is
;;; represented as a Lisp keyword. The defined types so far are:
;;;
;;;   :spend -- for an uncloaked spend transaction
;;;   :spend-cloaked -- for a cloaked spend transaction
;;;   :collect -- for a collect transaction
;;;   :coinbase -- for the (one) transaction that creates coin (a/k/a the genesis transaction)


;;;; Transaction Objects

;;; When you use a transaction (TX), you always completely consume its
;;; output.  Any amount not sent to some other party is conventionally
;;; sent back to the sender's same address, which is then referred to
;;; as a so-called "change address".

(defclass transaction ()
  ((type
    :reader type
    :initarg :type
    :initform :spend)
   (outputs             ;; sequence of transaction-output structures (see below)
    :reader outputs
    :initarg :outputs)
   (inputs              ;; sequence of transaction-input structures (see below)
    :reader inputs
    :initarg :inputs)))

   ;; (lock-time :initform 0))) ; not yet used, planned for later use ala Bitcoin

;; ---!!! TODO FIX: we currently have variant of segregating signature
;; ---!!! ("witness") data ala Bitcoin Segwit soft fork.  We are currently not
;; ---!!! including signature data in the regular hash of transaction data that
;; ---!!! gets signed (see serialize-transaction below). However, we need to
;; ---!!! follow through with the other side of "segwit", that is, to create
;; ---!!! "witness hash" (a "WTXID") consisting of both the transaction and
;; ---!!! signature data.  In Bitcoin under SegWit, the WTXID is used to compute
;; ---!!! the Witness merkle root, which is put in an output of the coinbase
;; ---!!! transaction of the block. In Bitcoin it is held in an output of the
;; ---!!! Coinbase transaction, but that was only done for compatibility: they
;; ---!!! would have put it in the block if they could have done so compatibly,
;; ---!!! but putting it in an output of the coinbase transaction was the next
;; ---!!! best thing.  Decide whether to keep or discard the actual signature
;; ---!!! data as opposed to its hash (or possibly to have that be optional per
;; ---!!! node).  -mhd, 6/8/18



;;; Classic Standard Bitcoin Transaction: Pay-to-Pubkey-Hash (P2PKH)
;;;
;;; Locking script on transaction output:
;;;
;;;   scriptPubKey: OP_DUP OP_HASH160 <pubKeyHash> OP_EQUALVERIFY OP_CHECKSIG
;;;
;;; Unlocking script on transaction input:
;;;
;;;   scriptSig: <sig> <pubKey>

;; Source:
;; https://en.bitcoin.it/wiki/Script#Standard_Transaction_to_Bitcoin_address_.28pay-to-pubkey-hash.29


;;; Newer Standard Bitcoin Transaction: Pay-to-Witness-Public-Key-Hash (P2WPKH)
;;;
;;; The newer transaction uses BIP 141 "Segregated Witness" (SegWit).
;;;

(defclass transaction-output ()
  ((address                     ;; like the Bitcoin address
    :reader output-address
    :initarg :address)
   (lock-script                 ;; what Bitcoin calls scriptPubKey
    :reader output-lock-script
    :initarg :lock-script)
   (amount                      ;; for uncloaked spend or collect transactions
    :reader output-amount              ;; integer amount of smallest coin units
    :initarg :amount)
   (proof                       ;; for cloaked spend transaction: proof, message
    :reader output-proof
    :initarg :proof)
   (message
    :reader output-message
    :initarg :message)))

(defclass transaction-input ()
  ((id
    :reader input-id
    :initarg :id)
   (index
    :reader input-index
    :initarg :index)
   (unlock-script                 ;; like Bitcoin scriptSig - list of args for lock script
    :reader input-unlock-script
    :initarg :unlock-script)
   (%public-key                   ;; direct pointers: in memory only: a local cache
    :initarg :%public-key
    :accessor %input-public-key)
   (%signature                    ;; detto -||- 
    :initarg :%signature
    :accessor %input-signature)))


(defun make-transaction (inputs outputs type)
  "Create a transaction with inputs TX-INPUTS, outputs TX-OUTPUTS, and
of type TYPE."
  (make-instance 'transaction
                 :type type
                 :inputs inputs
                 :outputs outputs))


(defmethod id= ((tx-id-1 vec-repr:bev) (tx-id-2 vec-repr:bev))
  "Return true if transaction IDs are equal. TX-ID-1 and TX-ID-2 must be transaction ID's, 
   which are represented as instances of class BEV (a class to represent a big-endian vector 
   of (unsigned-byte 8) elements), which in turn represents a sha-3/256 hash result."
  (equalp tx-id-1 tx-id-2))

(defmethod id= ((tx-id-1 string) (tx-id-2 string))
  (equal tx-id-1 tx-id-2))

(defmethod id= ((tx-id-1 vector) (tx-id-2 vector))
  (equalp tx-id-1 tx-id-2))


(defmethod id ((id vec-repr:bev))
  id)

(defmethod id ((tx transaction))
  "Get hash of TX, a transaction, which also uniquely* identifies it, as hash:hash/256 instance."
  (vec-repr:bev-vec (hash tx)))

(defmethod id ((str string))
  "Remove non-hex-digit characters and convert the resulting hex string to a txid byte vector."
  (ironclad:hex-string-to-byte-array
   (remove-if-not (lambda (char) (digit-char-p char 16)) str)))


(defmethod hash ((tx transaction))
  (hash:hash/256 (signature tx)))

(defun id-string (tx-id)
  "Return a string representation of TRANSACTION-ID, a byte vector, for display."
  (nstring-downcase (format nil (vec-repr:hex-str tx-id))))

(defmethod witness-data-p ((input transaction-input))
  (when (and (slot-boundp input '%signature) (%input-signature input)
             (slot-boundp input '%public-key) (%input-public-key input))
      t))

(defmethod sum-outputs ((tx transaction))
  (loop :for tx-out :in (outputs tx) :sum (output-amount tx-out)))


(defmethod precedes-p ((tx-1 transaction) (tx-2 transaction))
  "Predicate on two transactions T1 and T2. True if any input of T2
   refers to T1, meaning T1 must precede T2 on the blockchain."
  (let ((tx-1-id (id tx-1)))
    (loop :for tx-2-input :in (inputs tx-2)
       :thereis (id= tx-1-id tx-2-input))))


;;; SIGNATURE emit all the fields that should be hashed in a transaction.
;;; Note: for now [Mar 26, 2018], we just hash all inputs and all
;;; outputs. We must review and refine later to have this be parameterized to
;;; hash parts depending on what the situation calls for.

;;; In Bitcoin, there is a SIGHASH type set of bits with meanings as follows:
;;;
;;;   flag val    what to sign
;;;   all  0x01   all inputs, all outputs
;;;   none 0x02   all inputs, no outputs
;;;   one  0x03   all inputs, one output w/same index as signed input
;;;
;;; (So, in effect, what we now do below is the all case.)

;;; The parts of an output (UTXO) hashed are
;;;
;;;   public key hash
;;;   amount
;;;   lock script
;;;
;;; The parts of an input hashed are
;;;
;;;   ID (the hash of a previous transaction input)
;;;   index

;;; ---***!!! FIX: coinbase transactions could rarely result duplicate
;;; ---***!!! TXID. Bitcoin BIP 30 made those not allowed (via a
;;; ---***!!! network rule)
;;; ---***!!! https://github.com/bitcoin/bips/blob/master/bip-0030.mediawiki
;;; ---***!!! NB: for coinbase transactions, do as in Bitcoin BIP 34,
;;; ---***!!! which required coinbase transactions to include the
;;; ---***!!! height of the block they were mining in to their
;;; ---***!!! transaction data, so that coinbase transactions could be
;;; ---***!!! different.  This avoids the possibility of duplicate
;;; ---***!!! TXIDs. https://github.com/bitcoin/bips/blob/master/bip-0034.mediawiki#Motivation
;;; ---***!!! -mhd, 6/4/18
;;;
;;; Normally, you do not have block info - most normal transactions
;;; are created in wallet client.  Think about how to implement this
;;; feature. Only needed for case of creating a coinbase transaction,
;;; created by miners/minters when a block is created.

(defmethod signature ((tx transaction))
  (with-output-to-string (out)
    (flet ((emit (x)
             (format out "~a " x)))
      (loop
         :for tx-out :in (outputs tx)
         :as address = (output-address tx-out)      ; base58 string
         :as amount = (output-amount tx-out)        ; integer
         :as script = (output-lock-script tx-out)   ; chainlisp form
         :do (emit address) (emit amount) (emit script))
      (loop
         :for tx-in :in (inputs tx)
         :as id = (input-id tx-in)        ; string of 0's (coindbase case) or octet vector
         :as id-hex-string = (vec-repr:hex-str id) ; better: emit this
         :as index = (input-index tx-in)  ; integer
         :do (emit id-hex-string) (emit index)))))



(defun dump (tx &key out-only)
  (unless (eq (type tx) :spend)
    (emotiq:note "  ~a Transaction" (type tx)))
  (emotiq:note "  TxID: ~a" (id-string (id tx)))
  (unless (or out-only (member (type tx) '(:coinbase :collect))) ; no-input tx types
    (loop :for tx-in :in (inputs tx)
       :do (emotiq:note "    input outpoint: index = ~a/TxID = ~a"
                        (input-index tx-in) (id-string (input-id tx-in)))))
  (emotiq:note "    outputs:")
  (loop :for tx-out :in (outputs tx)
     :as i :from 0
     :do (emotiq:note "      [~d] amt = ~a (out to) addr = ~a"
                      i (output-amount tx-out) (output-address tx-out))))

