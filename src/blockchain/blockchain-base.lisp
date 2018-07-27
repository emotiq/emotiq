(in-package :emotiq/blockchain)

;;;; Genesis Block Creation

;;; In Bitcoin the first transaction of a block is created by a miner, and it's
;;; a special transaction with one input that's very wacky called a coinbase
;;; transaction with a so-called coinbase input. While every other transaction
;;; on the blockchain has as input a reference to a previous transaction, a
;;; coinbase transaction refers to a "null" transaction.

;;; Coinbase transaction are the main way coin gets into the system in
;;; Bitcoin. The other way is through transaction fees. In Bitcoin the miner who
;;; creates the block creates the coinbase transaction and is awarded the mining
;;; fee. The coinbase transaction issues one output to one address and that is
;;; to the miner's "wallet", i.e, their account (i.e., designated by their
;;; public key hash), and that is in the amount of the current block reward,
;;; which famously know gets halved every four years (currently [2018] 12.5
;;; BTC). So that's mining, and Emotiq does not feature mining.

;;; In addition to the mining reward, the coinbase transaction includes fees for
;;; the miner. That's a way for Bitcoin to pay a little more besides mining
;;; fees, and will be the only way miners get paid after miner awards approach
;;; and eventually reach zero.

;;; Now, since we, Emotiq, do not feature mining, what do we do at block
;;; creation time with the first transaction. I.e., how does the main amount of
;;; coin get into the system? Here is the answer.

;;; The Emotiq Whitepaper states that an initial supply of 10^9 coins will be
;;; created when EMTQ mainnet is launched. So, the genesis block would contain
;;; transactions paying 10^9 to one address (TBD), and from there onto others as
;;; desired.


(defmacro do-blockchain ((block-var) &body body)
  "Do blocks of the blockchain in reverse chronological order. Iterate over the
   blocks of the blockchain (i.e., the value of cosi-simgen:*blockchain*)
   beginning the most recent minted and ending with the genesis block, with
   BLOCK-VAR bound during each iteration to the block."
  `(loop :for ,block-var :in cosi-simgen:*blockchain*
      :do (progn ,@body)))

(defmacro do-transactions ((tx-var blk) &body body)
  "Do transactions of BLK in reverse chronological order. Bind each transaction
   of block BLK to TX-VAR in order from latest spent to earliest spent around
   the execution of BODY. It's possible to return using RETURN although
   RETURN-FROM to a lexical tag is recommended as the most reliable and clear
   method."
  (alexandria:with-gensyms (reversed-txs)
    `(let ((,reversed-txs (reverse (block:transactions ,blk))))
       (loop :for ,tx-var :in ,reversed-txs
          :do (progn ,@body)))))

(defmacro do-all-transactions ((tx-var &optional block-var?) &body body)
  "Iterate over all transactions of the blockchain from latest spent
   to earliest spent around the exectution of BODY. To exit early you
   must return using RETURN-FROM to a lexical tag."
  (let ((block-var (or block-var? (gensym "EMTQ-BLOCK"))))
    `(do-blockchain (,block-var)
       (do-transactions (,tx-var ,block-var)
         ,@body))))


(defun count-transactions (&key (types '(:spend)))
  (let ((count 0))
    (do-all-transactions (tx)
      (when (member (txn:type tx) types)
        (incf count)))
    count))

(defun dump (&key file mempool block blockchain node)
  (flet ((dump-loops ()
           (when block
             (emotiq:note "Dump txs in block = ~s:" block)
             (do-transactions (tx block)
               (txn:dump tx)))
           (when mempool
             (emotiq:note "Dump txs in mempool:")
             (loop :for tx :being :each :hash-value :of cosi-simgen:*mempool*
                :do (txn:dump tx)))
           (when blockchain
             (emotiq:note "Dump txs on blockchain:")
             (do-blockchain (block)
               (emotiq:note " Dump txs in block = ~s:" block)
               (do-transactions (tx block)
                 (txn:dump tx))))))
    (let ((cosi-simgen:*current-node*
           (or node 
               cosi-simgen:*current-node*
               cosi-simgen:*top-node*)))
      (if file
          (with-open-file (*standard-output*
                           file
                           :direction :output :if-exists :supersede)
            (dump-loops))
          (dump-loops)))))
