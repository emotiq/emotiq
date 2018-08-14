(in-package :emotiq/app)

;; a txn-list is a list of transactions that touched a given wallet account (all transactions created by the wallet account, all transactions which sent tokens to the account)
;; txn-lists consist of 5 items for each transaction):
;;   tx timestamp epoch kind fee


#|
sample from emotiq/src/model/mock.lisp

(defparameter *transactions*
  (let ((address "ytD4ccGjrA1hRgYXiEvA8GMS8s9NK553Q2"))        <-- wallet address
    `(:array
      (:object (:id . "C9746DE0C63763A650FCA")                 <-- transaction id
               (:timestamp . 1527591324)                       <-- transaction details
               (:type . "spend")
               (:epoch . 314)
               (:fee . 10)
               (:inputs .                                      <-- inputs (:array)
                        (:array
                         (:object                              <-- an input
                          (:cloaked . (:false))                <-- always uncloaked for MVP (testnet: ask David, every wallet must send first txn cloaked)
                          (:address . ,address)                <-- address of sender?  (should this be the txid of originating txn?)
                          (:amount . 10000))))                 <-- amount sent to this input
               (:outputs .                                     <-- outputs of this txn
                         (:array
                          (:object                             <-- an output
                           (:cloaked . (:false))               <-- always uncloaked (for now)
                           (:address . ,address)               <-- address of receiver
                           (:amount . 9000))                   <-- amount sent
                          (:object
                           (:cloaked . (:false))
                           (:address . "hRgYXiEvA8GMS8s9NK553Q2123123")
                           (:amount . 1000)))))
      (:object (:id . "277A27C6C9746DE0C63763A650FCA")
               (:timestamp . 1527691324) (:type . "spend") (:epoch . 315) (:fee . 10)
               (:inputs .
                        (:array
                         (:object
                          (:cloaked . (:false))
                          (:address . ,address)
                          (:amount . 500))))
               (:outputs .
                         (:array
                          (:object
                           (:cloaked . (:false))
                           (:address . "hRgYXiEvA8GMS8s9NK553Q2123123")
                           (:amount . 400))
                          (:object
                           (:cloaked . (:false))
                           (:address . ,address)
                           (:amount . 100)))))
      (:object (:id . "277A27C6C9746DE0C63763A6CA")
               (:timestamp . 1527691324) (:type . "spend") (:epoch . 315) (:fee . 10)
               (:inputs .
                        (:array
                         (:object
                          (:cloaked . (:false))
                          (:address . ,address)
                          (:amount . 50000))))
               (:outputs .
                         (:array
                          (:object
                           (:cloaked . (:false))
                           (:address . "ypsrE7nAMx96T5QavqLxoLVp26MMpGEQSZ")
                           (:amount . 40000))
                          (:object
                           (:cloaked . (:false))
                           (:address . "hRgYXiEvA8GMS8s9NK553Q2123123")
                           (:amount . 4000))
                          (:object
                           (:cloaked . (:false))
                           (:address . ,address)
                           (:amount . 100)))))
      (:object (:id . "9746DE0C63763A650FCAFF277A27C6C")
               (:timestamp . 1527791324) (:type . "spend") (:epoch . 320) (:fee . 10)
               (:inputs .
                        (:array
                         (:object
                          (:cloaked . (:false))
                          (:address . ,address)
                          (:amount . 10000))))
               (:outputs .
                         (:array
                          (:object
                           (:cloaked . (:false))
                           (:address . "ywo9Y6yRrHhKqsJyuVcEWj5H1df1MKZMbe")
                           (:amount . 9000))
                          (:object
                           (:cloaked . (:false))
                           (:address . ,address)
                           (:amount . 1000))))))))
|#


(defun test-alist (wallet-address)
  (let ((txn-list (get-transactions wallet-address)))
    `(:array
    ;  (:object
       ;(:wallet-id . ,wallet-address)  ;; added 'wallet-' for debug
       ;(:array
    ,(convert-transactions-to-alist txn-list))))

(defun convert-transactions-to-alist (txn-list)
  "convert txn tuples (tx timestamp epoch kind fee) into alists for json"
  (mapcar #'convert-one-transaction-to-alist-from-tuple txn-list))

(defun txid-long-string (transaction-id)
  "Return a string representation of TRANSACTION-ID, a byte vector,
   for display. The result is a lowercase hex string."
  (nstring-downcase (format nil (pbc::addr-str transaction-id))))

(defun convert-one-transaction-to-alist-from-tuple (tuple)
  (destructuring-bind (tx timestamp epoch kind fee)
      tuple
    (let ((ins (cosi/proofs/newtx:transaction-inputs tx))
          (outs (cosi/proofs/newtx:transaction-outputs tx)))
      (let ((*print-readably* t))
        (let ((in-alist `(:inputs .
                          (:array ,(mapcar #'convert-input-to-alist ins))))
              (out-alist `(:outputs .
                           (:array ,(mapcar #'convert-output-to-alist outs)))))
          `(:object
            (:id . ,(txid-long-string (get-txid tx))) ;; added 'transaction-' for debug
            (:timestamp . ,timestamp)
            (:epoch . ,epoch)
            (:type. ,kind)
            (:fee . ,fee)
            ,in-alist
            ,out-alist))))))

(defun convert-input-to-alist (i)
  `(:object
    (:cloaked . (:false))
    (:address . ,(pbc:addr-str (get-address-of-sender-to-input i)))
    (:amount . ,(amount-in-input i))))

(defun convert-output-to-alist (o)
  `(:object
    (:cloaked . (:false))
    (:address . ,(pbc:addr-str (payee-address o)))
    (:amount . ,(amount-paid o))))

    