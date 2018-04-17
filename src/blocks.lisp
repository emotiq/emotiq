;;;; blocks.lisp

(in-package :emotiq)




                                                                                ;
(defclass block ()
  ((protocol-version
    :accessor protocol-version
    :initform 1
    :documentation 
      "Version of the protocol/software, an integer")

   (epoch                               ; aka "height"
    :initarg :epoch :accessor epoch
    :initform nil 
    :documentation
      "The integer of the epoch that this block is a part of."
      ;; height: "Position of block in the chain, an integer."
      )

   (prev-block
    :accessor prev-block
    :documentation "Previous block (nil for genesis block).")
   (prev-block-hash
    :accessor prev-block-hash
    :documentation "Hash of previous block (nil for genesis block).")

   (merkle-root-hash
    :accessor merkle-root-hash
    :documentation "Merkle root hash of block-transactions.")

   (block-timestamp
    :accessor block-timestamp
    :documentation 
      "Approximate creation time in seconds from January 1, 1900 GMT")

   ;; Block-transactions is generally what's considered the main
   ;; contents of a block whereas the rest of the above comprises
   ;; what's known as the 'block header' information.
   (transactions
    :accessor transactions
    :documentation "Sequence of transactions")

   ;; Caller can set and maintain these slots. These could be set to
   ;; contain a treap or possibly for now just a simple list.
   (validator-keys-joining
    :accessor validator-keys-joining
    :documentation "The validators nodes joining in this epoch.")
   (validator-keys-leaving
    :accessor validator-keys-leaving
    :documentation "The validator nodes leaving in this epoch."))


  (:documentation "A block in the Emotiq chain."))



(defun create-block (epoch prev-block transactions)
  (let ((block (make-instance 'block :epoch epoch)))
    (setf (prev-block block) prev-block)
    (setf (prev-block-hash block) 
          (if (null prev-block)
              nil
              (hash-block prev-block)))
    (setf (merkle-root-hash block)
          (compute-merkle-root-hash transactions))
    (setf (transactions block) transactions)
    (setf (block-timestamp block) (get-universal-time))
    block))



(defun hash-block (block)
  (hash-256-string (serialize-block block)))

(defun serialize-block (block)  
  (with-output-to-string (out)
    (flet ((emit (x)
             (format out "~a " x)))
      (emit (protocol-version block))
      (emit (epoch block))
      (emit (prev-block-hash block))
      (emit (merkle-root-hash block)))))



;;; COMPUTE-MERKLE-ROOT-HASH: construct a merkle root for a list of
;;; TRANSACTIONS according to bitcoin.org Bitcoin developer reference
;;; doc, here:
;;; https://bitcoin.org/en/developer-reference#block-versions

(defun compute-merkle-root-hash (transactions)
  (compute-merkle
   (loop for tx in transactions
        as txid = (transaction-id tx)
         collect txid)
   #'hash-256-string))

(defun compute-merkle (nodes hash-fn)
  "Compute merkle root hash on nonempty list of hashables NODES."
  (assert (not (null nodes)) () "NODES must not be null.")
  (cond
    ((null (rest nodes))                ; just 1
     (funcall hash-fn (first nodes)))
    (t (compute-merkle-pairs nodes hash-fn))))


(defun compute-merkle-pairs (nodes hash-fn)
  (flet ((hsh (x) (funcall hash-fn x))
         (pair (a b) (concatenate 'string a b)))
    (if (null (rest (rest nodes)))
        ;; 2 left
        (hsh (pair (first nodes) (second nodes)))
        ;; three or more:
        (loop for (a b . rest?) on nodes by #'cddr
              when (and (null rest?) (null b))
                do (setq b a) ; odd-length row case: duplicate last row item
              collect (hsh (pair a b))
                into row
              finally (return (compute-merkle row hash-fn))))))
