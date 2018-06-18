;; cosi-handlers.lisp -- Handlers for various Cosi operations
;;
;; DM/Emotiq  02/18
;; ---------------------------------------------------------------
#|
The MIT License

Copyright (c) 2018 Emotiq AG

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :cosi-simgen)
;; ---------------------------------------------------------------

(defun NYI (&rest args)
  (error "Not yet implemented: ~A" args))

(defvar *cosi-prepare-timeout* 10
  "Timeout in seconds that the leader waits during prepare phase for sealing a block.")

(defvar *cosi-commit-timeout* 10
  "Timeout in seconds that the leader waits during commit phase for sealing a block.")

;; -------------------------------------------------------

(defvar *current-node*  nil)  ;; for sim = current node running

(defun current-node ()
  *current-node*)

(define-symbol-macro *blockchain*     (node-blockchain *current-node*))
(define-symbol-macro *blockchain-tbl* (node-blockchain-tbl *current-node*))
(define-symbol-macro *mempool*        (node-mempool *current-node*))
(define-symbol-macro *utxo-table*     (node-utxo-table *current-node*))
(define-symbol-macro *leader*         (node-current-leader *current-node*))
(define-symbol-macro *holdoff*        (node-hold-off *current-node*))
(define-symbol-macro *tx-changes*     (node-tx-changes *current-node*))

(defstruct tx-changes
  ;; all lists contain key vectors
  tx-dels    ;; list of pending transation removals from mempool
  utxo-adds  ;; list of pending new utxos
  utxo-dels) ;; list of pending spent utxos

;; -------------------------------------------------------

(defgeneric node-dispatcher (msg-sym &key &allow-other-keys))

(defmethod node-dispatcher (msg-sym &key)
  (error "Unknown message: ~A~%Node: ~A" msg-sym (short-id *current-node*)))

(defun end-holdoff ()
  (ac::unschedule-timer (node-hold-off-timer *current-node*))
  (setf *holdoff* nil))

(defun set-holdoff ()
  (setf *holdoff* t)
  (ac::schedule-timer-relative (node-hold-off-timer *current-node*)
                               (+ *cosi-prepare-timeout*
                                  *cosi-commit-timeout*
                                  10)))

(defmethod node-dispatcher ((msg-sym (eql :end-holdoff)) &key)
  (end-holdoff))

(defmethod node-dispatcher ((msg-sym (eql :become-leader)) &key)
  (setf *tx-changes* (make-tx-changes))
  (set-holdoff))

(defmethod node-dispatcher ((msg-sym (eql :become-witness)) &key)
  (setf *tx-changes* (make-tx-changes))
  (set-holdoff))

(defmethod node-dispatcher ((msg-sym (eql :reset)) &key)
  (reset-nodes))

(defmethod node-dispatcher ((msg-sym (eql :answer)) &rest args)
  (ac:pr args))

(defmethod node-dispatcher ((msg-sym (eql :genesis-utxo)) &key utxo)
  (pr (format nil "~A got genesis utxo" (short-id (current-node))))
  (really-record-new-utxo utxo))

;; for new transactions:  -mhd, 6/12/18
(defmethod node-dispatcher ((msg-sym (eql :genesis-block)) &key blk)
  (pr (format nil "~A got genesis block" (short-id (current-node))))
  (unless blk ; Why are we using optional args. ???  -mhd, 6/12/18
    (error "BLK is nil, can't continue."))
  (push blk *blockchain*)
  (setf (gethash (cosi/proofs:hash-block blk) *blockchain-tbl*) blk))

;; for new transactions:  -mhd, 6/12/18
(defmethod node-dispatcher ((msg-sym (eql :new-transaction-new)) &key trn)
  (cosi/proofs/newtx:validate-transaction trn))

(defmethod node-dispatcher ((msg-sym (eql :make-block)) &key)
  (leader-exec *cosi-prepare-timeout* *cosi-commit-timeout*))

(defmethod node-dispatcher ((msg-sym (eql :cosi-sign-prepare)) &key reply-to blk timeout)
  (node-compute-cosi reply-to :prepare blk timeout))

(defmethod node-dispatcher ((msg-sym (eql :cosi-sign-commit)) &key reply-to blk timeout)
  (node-compute-cosi reply-to :commit blk timeout))

(defmethod node-dispatcher ((msg-sym (eql :new-transaction)) &key trn)
  (node-check-transaction trn))

(defmethod node-dispatcher ((msg-sym (eql :public-key)) &key reply-to)
  (reply reply-to :pkey+zkp (node-pkeyzkp *current-node*)))

(defmethod node-dispatcher ((msg-sym (eql :election)) &key new-leader-pkey)
  (node-elect-new-leader new-leader-pkey))

(defmethod node-dispatcher ((msg-sym (eql :signing)) &key reply-to consensus-stage blk seq timeout)
  (node-cosi-signing reply-to
                     consensus-stage blk seq timeout))

(defmethod node-dispatcher ((msg-sym (eql :add/change-node)) &key new-node-info)
  (node-insert-node new-node-info))

(defmethod node-dispatcher ((msg-sym (eql :remove-node)) &key node-pkey)
  (node-remove-node node-pkey))

(defmethod node-dispatcher ((msg-sym (eql :block-finished)) &key)
  (ac:pr "Block committed to blockchain")
  (ac:pr (format nil "Block signatures = ~D"
                 (logcount (block-signature-bitmap (first *blockchain*))))))

;; ------------------------------------------------------------------------------------

(defun make-node-dispatcher (node)
  (let ((beh  (make-actor
               (lambda (&rest msg)
                 (let ((*current-node* node))
                   (apply 'node-dispatcher msg)))
               )))
    (make-actor
     (lambda (&rest msg)
       (um:dcase msg
         (:actor-callback (aid &rest ans)
          (let ((actor (lookup-actor-for-aid aid)))
            (when actor
              (apply 'send actor ans))
            ))
         
          (t (&rest msg)
             (apply 'send beh msg))
          )))))
        
(defun crash-recovery ()
  ;; just in case we need to re-make the Actors for the network
  (maphash (lambda (k node)
             (declare (ignore k))
             (setf (node-self node) (make-node-dispatcher node)))
           *ip-node-tbl*))

;; -------------------------------------------------------
;; New leader node election... tree rearrangement

(defun notify-real-descendents (node &rest msg)
  (labels ((recurse (sub-node)
             (if (node-realnode sub-node)
                 (apply 'send sub-node msg)
               (iter-subs sub-node #'recurse))))
    (iter-subs node #'recurse)))

(defun all-nodes-except (node)
  (delete node
          (um:accum acc
            (maphash (um:compose #'acc 'um:snd) *ip-node-tbl*))))

(defun node-model-rebuild-tree (parent node nlist)
  (let ((bins (partition node nlist
                         :key 'node-ip)))
    (iteri-subs node
                (lambda (ix subs)
                  (setf (aref bins ix)
                        (node-model-rebuild-tree node
                                                 (car subs)
                                                 (cdr subs)))))
    (setf (node-parent node) parent)
    (set-node-load node)
    node))

(defun node-elect-new-leader (new-leader-pkey)
  (let ((new-top-node (gethash (int new-leader-pkey) *pkey-node-tbl*)))
    ;; Maybe... ready for prime time?
    (cond ((null new-top-node)
           (error "Not a valid leader node: ~A" new-leader-pkey))
          ((eq new-top-node *top-node*)
           ;; nothing to do here...
           )
          (t
           (setf *top-node* new-top-node)
           (node-model-rebuild-tree nil new-top-node
                                    (all-nodes-except new-top-node))
           ;;
           ;; The following broadcast will cause us to get another
           ;; notification, but by then the *top-node* will already
           ;; have been set to new-leader-ip, and so no endless loop
           ;; will occur.
           ;;
           (notify-real-descendents new-top-node :election new-leader-pkey))
          )))

;; ---------------------------------------------------------
;; Node insertion/change

(defun bin-for-ip (node ip)
  (let ((vnode  (dotted-string-to-integer (node-ip node)))
        (vip    (dotted-string-to-integer ip)))
    (mod (logxor vnode vip) (length (node-subs node)))))

(defun increase-loading (parent-node)
  (when parent-node
    (incf (node-load parent-node))
    (increase-loading (node-parent parent-node))))

(defun node-model-insert-node (node new-node-info)
  ;; info is (ipv4 port pkeyzkp)
  (destructuring-bind (ipstr pkeyzkp) new-node-info
    (let* ((ix       (bin-for-ip node ipstr))
           (bins     (node-subs node))
           (sub-node (aref bins ix)))
      (if sub-node
          ;; continue in parallel with our copy of tree
          (node-model-insert-node sub-node new-node-info)
        ;; else
        (let ((new-node (make-node ipstr pkeyzkp node)))
          (setf (node-real-ip new-node)  ipstr
                (node-skey new-node)     nil
                (aref bins ix)           new-node)
          (incf (node-load node))
          (increase-loading (node-parent node)))
        ))))

(defun node-insert-node (new-node-info)
  (destructuring-bind (ipstr port pkeyzkp) new-node-info
    (declare (ignore port)) ;; for now...
    (let* ((node *current-node*)
           (new-node (gethash ipstr *ip-node-tbl*)))
      (if new-node ;; already present in tree?
          ;; maybe caller just wants to change keying
          ;; won't be able to sign unless it know skey
          (multiple-value-bind (pkey ok) (check-pkey pkeyzkp)
            (when ok
              (setf (node-pkeyzkp new-node)  pkeyzkp
                    (node-pkey new-node)     pkey ;; cache the decompressed key
                    (node-real-ip new-node)  ipstr)))
        ;; else - not already present
        (node-model-insert-node *top-node* new-node-info))
      (notify-real-descendents node :insert-node new-node-info))))

;; ---------------------------------------------------------

(defun node-model-remove-node (gone-node)
  (remhash (node-ip gone-node) *ip-node-tbl*)
  (let ((pcmpr (keyval (first (node-pkeyzkp gone-node)))))
    (remhash pcmpr *pkey-node-tbl*)
    (remhash pcmpr *pkey-skey-tbl*)))

(defun node-remove-node (gone-node-pkey)
  (let* ((node *current-node*)
         (gone-node (gethash (int gone-node-pkey) *pkey-node-tbl*)))
    (when gone-node
      (node-model-remove-node gone-node)
      ;; must rebuild tree to absorb node's subnodes
      (node-model-rebuild-tree nil *top-node*
                               (all-nodes-except *top-node*))
      (when (eq node *top-node*)
        (notify-real-descendents node :remove-node gone-node-pkey)))))
  
#|
(send *top-node* :public-key (make-node-ref *my-node*))
==> see results in output window
(:PKEY+ZKP (849707610687761353988031598913888011454228809522136330182685594047565816483 77424688591828692687552806917061506619936267795838123291694715575735109065947 2463653704506470449709613051914446331689964762794940591210756129064889348739))

COSI-SIMGEN 23 > (send (gethash "10.0.1.6" *ip-node-tbl*) :public-key (make-node-ref *my-node*))

Connecting to #$(NODE "10.0.1.6" 65000)
(FORWARDING "10.0.1.6" (QUOTE ((:PUBLIC-KEY #<NODE-REF 40200014C3>) 601290835549702797100992963662352678603116278028765925372703953633797770499 56627041402452754830116071111198944351637771601751353481660603190062587211624 23801716726735741425848558528841292842)))
==> output window
(:PKEY+ZKP (855676091672863312136583105058123818001884231695959658747310415728976873583 19894104797779289660345137228823739121774277312822467740314566093297448396984 2080524722754689845098528285145820902670538507089109456806581872878115260191))
|#
#|
(defun ptst ()
  ;; test requesting a public key
  (spawn
   (lambda ()
     (let* ((my-ip    (node-real-ip *my-node*))
            (my-port  (start-ephemeral-server))
            (ret      (current-actor)))
         (labels
             ((exit ()
                (become 'do-nothing)
                (shutdown-server my-port)))
           (pr :my-port my-port)
           #+:LISPWORKS (inspect ret)
           (send *my-node* :public-key ret)
           (recv
             (msg
              (pr :I-got... msg)
              (exit))
             :TIMEOUT 2
             :ON-TIMEOUT
             (progn
               (pr :I-timed-out...)
               (exit))
             ))))
   ))

(defun stst (msg)
  ;; test getting a signature & verifying it
  (spawn
   (lambda ()
     (let* ((my-ip    (node-real-ip *my-node*))
            (my-port  (start-ephemeral-server))
            (ret      (current-actor)))
       (labels
           ((exit ()
              (become 'do-nothing)
              (shutdown-server my-port)))
         (pr :my-port my-port)
         #+:LISPWORKS (inspect ret)
         (send *top-node* :cosi-sign ret msg)
         (recv
           ((list :answer (and packet
                               (list :signature _ sig)))
            (pr :I-got... packet)
            (pr (format nil "Witnesses: ~A" (logcount (um:last1 sig))))
            (send *my-node* :validate ret msg sig)
            (recv
              (ansv
               (pr :Validation ansv)
               (exit))
              :TIMEOUT 1
              :ON-TIMEOUT
              (pr :timed-out-on-signature-verification)
              (exit)))
           
           (xmsg
            (pr :what!? xmsg)
            (exit))
           
           :TIMEOUT 15
           :ON-TIMEOUT
           (progn
             (pr :I-timed-out...)
             (exit))
           ))))
   ))
|#

(defvar *election-proof*   nil)

(defvar *max-transactions*  16)  ;; max nbr TX per block
(defvar *in-simulatinon-always-byz-ok* nil) ;; set to nil for non-sim mode, forces consensus

(defun signature-hash-message (blk)
  (cosi/proofs:serialize-block-header-octets blk))

(defun get-block-transactions (blk)
  "Right now it is a simple partially ordered list of transactions.
Later it may become an ADS structure"
  (cosi/proofs:block-transactions blk))

;; --------------------------------------------------------------------

(defmethod node-check-transaction (msg)
  "Just ignore invalid messages"
  nil)

(defmethod node-check-transaction ((msg transaction))
  (when (check-transaction-math msg)
    (let ((key (bev-vec (hash/256 msg))))
      (when (gethash key *mempool*)
        (error "Duplicate transaction delivery"))
      (setf (gethash key *mempool*) msg))))

;; -------------------------------
;; testing-version transaction cache

;; MEMPOOL is only affected by Leader node at block assembly time,
;; and by all witness nodes only at end of successful commit phase.

(defmethod remove-tx-from-mempool ((key bev))
  "Record tx to be removed at sync"
  (push (bev-vec key) (tx-changes-tx-dels *tx-changes*)))

(defmethod remove-tx-from-mempool ((tx transaction))
  (remove-tx-from-mempool (bev (hash/256 tx))))

(defun replay-remove-txs-from-mempool (blk)
  "Called at successful commit to remove transaction from mempool.
Here, we rebuild the lists of UTXO changes to ensure that everyone is
playing on the same field.

It doesn't matter whether witness node agreed during :PREPARE phase of
consensus. As long as the :COMMIT phase has been entered, and
everything up to here checks out, we play the changes to the utxo
database indicated by the transactions in the block."
  (reset-transaction-changes)
  (dolist (tx (get-block-transactions blk))
    (remove-tx-from-mempool tx)
    (dolist (txin (trans-txins tx))
      (record-spent-utxo txin))
    (dolist (txout (trans-txouts tx))
      (record-new-utxo txout))))

(defun sync-database ()
  "Perform all pending transaction removals from mempool. Remove spent
utxos, add new utxos."
  (with-accessors ((tx-dels   tx-changes-tx-dels)
                   (utxo-dels tx-changes-utxo-dels)
                   (utxo-adds tx-changes-utxo-adds)) *tx-changes*
    (dolist (tx tx-dels)
      (remhash tx *mempool*))
    (dolist (utx utxo-dels)
      (remhash utx *utxo-table*))
    (dolist (utx utxo-adds)
      (setf (gethash utx *utxo-table*) :spendable))
    ))

;; -------------------------------
;; testing-version TXOUT log

(defmethod lookup-utxo ((key bev))
  "Lookup utxo by key. Return :spendable, :spent, or nil if missing"
  (with-accessors ((utxo-dels  tx-changes-utxo-dels)
                   (utxo-adds  tx-changes-utxo-adds)) *tx-changes*
    (let ((vkey (bev-vec key)))
      (cond ((find vkey utxo-dels
                   :test 'equalp)
             :spent)
            
            ((find vkey utxo-adds
                   :test 'equalp)
             :spendable)
            
            (t
             ;; return either nil if missing, or :spendable as present
             (gethash vkey *utxo-table*))
            ))))

(defmethod really-record-new-utxo ((txout txout))
  "KEY is Hash(P,C) of TXOUT - record tentative TXOUT. Once finalized,
they will be added to utxo-table"
  (let ((vkey (bev-vec (txout-hashlock txout))))
    (multiple-value-bind (x present-p)
        (gethash vkey *utxo-table*)
      (declare (ignore x))
      (when present-p
        (error "Shouldn't Happen: Effective Hash Collision!!!"))
      (setf (gethash vkey *utxo-table*) :spendable))))

(defmethod record-new-utxo ((txout txout))
  "Accumulate txout as new utxo"
  (with-accessors ((utxo-adds  tx-changes-utxo-adds)) *tx-changes*
    (let ((key (bev-vec (txout-hashlock txout))))
      (when (or (find key utxo-adds
                      :test 'equalp)
                (gethash key *utxo-table*))
        (error "Shouldn't happen: Effective hash collision"))
      (push key utxo-adds))))

(defmethod record-spent-utxo ((txin txin))
  "Record utxo key as spent"
  (with-accessors ((utxo-adds  tx-changes-utxo-adds)
                   (utxo-dels  tx-changes-utxo-dels)) *tx-changes*
    (let ((vkey (bev-vec (txin-hashlock txin))))
      (setf utxo-adds (delete vkey utxo-adds
                              :test 'equalp))
      (push vkey utxo-dels))))

(defmethod back-out-transaction ((tx transaction))
  "Unrecord UTXO create/destroy resulting from this transaction"
  (with-accessors ((utxo-adds  tx-changes-utxo-adds)
                   (utxo-dels  tx-changes-utxo-dels)) *tx-changes*
    (dolist (txin (trans-txins tx))
      (let ((vkey (bev-vec (txin-hashlock txin))))
        (when (find vkey utxo-dels
                    :test 'equalp)
          (setf utxo-dels (delete vkey utxo-dels
                                  :test 'equalp))
          (unless (gethash vkey *utxo-table*)
            (push vkey utxo-adds))
          )))
    
    (dolist (txout (trans-txouts tx))
      (let ((key (bev-vec (txout-hashlock txout))))
        (setf utxo-adds (delete key utxo-adds
                                :test 'equalp))
        ))))

(defun reset-transaction-changes ()
  "Reset the UTXO add/del for this session"
  (with-accessors ((utxo-adds  tx-changes-utxo-adds)
                   (utxo-dels  tx-changes-utxo-dels)) *tx-changes*
    (setf utxo-adds nil
          utxo-dels nil)))
  
;; -------------------------------------------------------------------
;; Code to check incoming transactions for self-validity, not
;; inter-transactional validity like double-spend

(defun txin-keys (tx)
  (mapcar (um:compose 'int 'txin-hashlock) (trans-txins tx)))

(defun txout-keys (tx)
  (mapcar (um:compose 'int 'txout-hashlock) (trans-txouts tx)))

(defun check-transaction-math (tx)
  "TX is a transaction. Check that no TXIN refers to one of the TXOUT.
Check that every TXIN and TXOUT has a valid range proof, and that the
sum of TXIN equals the sum of TXOUT.

If passes these checks, record the transaction and its hash in a pair
in a cache log to speed up later block validation.

Return nil if transaction is invalid."
  (let* ((key        (bev (hash/256 tx)))
         (txout-keys (txout-keys tx)))
    (when (and (notany (lambda (txin)
                         ;; can't be spending an output you are just
                         ;; now creating
                         (find txin txout-keys))
                       (txin-keys tx))
               ;; now do the math
               (validate-transaction tx))
      (ac:pr (format nil "Transaction: ~A checks: ~A"
                     (short-id key)
                     (short-id *current-node*)))
      t)))

;; --------------------------------------------------------------------
;; Code to assemble a block - must do full validity checking,
;; including double-spend checks

(defstruct txrec
  tx txkey ins outs)

(defun topo-sort (tlst)
  "Topological partial ordering of transactions. TXOUT generators need
to precede TXIN consumers.

TLST is a list of pairs (k v) with k being the hash of the
transaction, and v being the transaction itself."
  ;; first, compute lists of keys just once
  (pr "Topological sorting")
  (let ((txrecs (mapcar (lambda (tx)
                          (make-txrec
                           :tx     tx
                           :txkey  (bev (hash/256 tx))
                           :ins    (txin-keys tx)
                           :outs   (txout-keys tx)))
                        tlst)))
    (labels ((depends-on (a b)
               ;; return true if a depends on b
               (let ((ins  (txrec-ins a))
                     (outs (txrec-outs b)))
               (some (um:rcurry 'member outs) ins)))
             
             (depends-on-some (a lst)
               ;; true if a depends on some element of lst
               (some (um:curry #'depends-on a) lst)))
      
      (mapcar 'txrec-tx ;; convert back to incoming tx
              (um:accum acc
                (um:nlet-tail outer ((lst txrecs))
                  (when lst
                    (let ((rem (um:nlet-tail iter ((lst  lst)
                                                   (rest nil))
                                 (if (endp lst)
                                     rest
                                   (let ((hd (car lst))
                                         (tl (cdr lst)))
                                     (cond ((depends-on hd hd)
                                            ;; if we depend on our own outputs,
                                            ;; then invalid TX
                                            (remove-tx-from-mempool (txrec-txkey hd))
                                            (iter tl rest))
                                           
                                           ((depends-on-some hd (append tl rest))
                                            ;; try next TX
                                            (iter tl (cons hd rest)))
                                           
                                           (t
                                            ;; found a TX with no dependencies on rest of list
                                            (acc hd)
                                            (iter tl rest))
                                           ))
                                   ))))
                      (if (= (length lst) (length rem))
                          ;; no progress made - must be interdependencies
                          (dolist (rec rem)
                            ;; discard TX with circular dependencies and quit
                            (remove-tx-from-mempool (txrec-txkey rec)))
                        ;; else -- try for more
                        (outer rem))
                      ))))))))

(defun check-double-spend (tx)
  "TX is transaction in current pending block.  Check every TXIN to be
sure no double-spending, nor referencing unknown TXOUT. Return nil if
invalid TX."
  (labels ((txin-ok (txin)
             (let* ((key  (bev (txin-hashlock txin)))
                    (utxo (lookup-utxo key)))

               (cond ((null utxo)
                      (error (format nil "*** non-existent UTXO spend ~A" tx))
                      nil)
                     
                     ((eq :spendable utxo)
                      (record-spent-utxo txin)
                      t)

                     (t
                      (error (format nil "*** double spend txn ~A" tx)) ;; TODO: 157602158
                      nil)
                     ))))
    (cond ((every #'txin-ok (trans-txins tx))
           (dolist (txout (trans-txouts tx))
             (record-new-utxo txout))
           t)
          
          (t
           ;; remove transaction from mempool
           (back-out-transaction tx)
           (remove-tx-from-mempool tx)
           nil)
          )))

(defun get-candidate-transactions ()
  "Scan available TXs for numerically valid, spend-valid, and return
topo-sorted partial order"
  (let ((txs  nil))
    (maphash (lambda (k tx)
               (declare (ignore k))
               (push tx txs))
             *mempool*)
    (let ((trimmed (topo-sort txs)))
      (dolist (tx (set-difference txs trimmed))
        ;; remove invalid transactions whose inputs refer to future
        ;; outupts
        (remove-tx-from-mempool tx))
      ;; checking for double spending also creates additional UTXO's.
      (um:accum acc
        (dolist (tx trimmed)
          (when (check-double-spend tx)
            (acc tx)))) ;; accumulate just the TX, not the key too
      )))
               
(defun get-transactions-for-new-block ()
  (when *newtx-p*
    (return-from get-transactions-for-new-block
      (cosi/proofs/newtx:get-transactions-for-new-block
       :max *max-transactions*)))
  (let ((tx-pairs (get-candidate-transactions)))
    (pr "Trimming transactions")
    (multiple-value-bind (hd tl)
        (um:split *max-transactions* tx-pairs)
      (dolist (tx tl)
        ;; put these back in the pond for next round
        (back-out-transaction tx))
      ;; now hd represents the actual transactions going into the next block
      (pr (format nil "~D Transactions" (length hd)))
      hd)))
      
;; ----------------------------------------------------------------------
;; Code run by Cosi block validators...

(defun #1=check-block-transactions (blk)
  "TLST is list of transactions from current pending block. Return nil
if invalid block.

List of TX should already have been topologically sorted so that input
UTXO's were created in earlier transactions or earlier blocks in the
blockchain.

Check that there are no forward references in spend position, then
check that each TXIN and TXOUT is mathematically sound."
  (when *newtx-p*
    (return-from #1#
      (cosi/proofs/newtx:check-block-transactions blk)))
  (dolist (tx (get-block-transactions blk))
    (dolist (txin (trans-txins tx))
      (let ((key  (bev (txin-hashlock txin))))
        (unless (eql :SPENDABLE (lookup-utxo key))
          (return-from #1# nil)) ;; caller must back out the changes made so far...
        (record-spent-utxo txin)))   ;; mark as spend in this TX
    ;; now check for valid transaction math
    (unless (check-transaction-math tx)
      (return-from #1# nil))
    ;; add-method TXOUTS to UTX table
    (dolist (txout (trans-txouts tx))
      (record-new-utxo txout)))
  t) ;; tell caller everything ok

;; --------------------------------------------------------------------
;; Message handlers for verifier nodes

;; -----------------------------------------------------------------------

#-(AND :COM.RAL :LISPWORKS)
(defparameter *dly-instr*
  (ac:make-actor
   (lambda (&rest args)
     (declare (ignore args))
     t)))

#+(AND :COM.RAL :LISPWORKS)
(defparameter *dly-instr*
  ;; Very useful for timeout tuning. If timeouts are properly set,
  ;; then histogram will be entirely to left of red 1.0 Ratio, but not
  ;; too far left
  (ac:make-actor
   (let ((data   nil)
         (pltsym :plt))
     (um:dlambda
       (:incr (dly)
        (push dly data))
       (:clr ()
        (setf data nil))
       (:pltwin (sym)
        (setf pltsym sym))
       (:plt ()
        (when data
          (plt:histogram pltsym data
                         :clear  t
                         :ylog   t
                         :xrange '(0 1.2)
                         :thick  2
                         ;; :cum    t
                         :norm   nil
                         :title  "Measured Delay Ratios"
                         :xtitle "Delay-Ratio"
                         :ytitle "Counts")
          (plt:plot pltsym '(1 1) '(0.1 1e6)
                    :color :red)))
       ))))

;; -----------------------------------------------------------------------

(defun msg-ok (msg node)
  (declare (ignore msg))
  (not (node-byz node))) ;; for now... should look at node-byz to see how to mess it up

(defun mark-node-no-response (node sub)
  (declare (ignore node sub)) ;; for now...
  nil)

(defun mark-node-corrupted (node sub)
  (declare (ignore node)) ;; for now...
  (setf (node-bad sub) t)
  nil)

;; -------------------------------------------------------------------
;; debug init

(defun reset-system ()
  (send-real-nodes :reset))

(defun reset-nodes ()
  (loop for node across *node-bit-tbl* do
        (setf (node-bad        node) nil
              (node-blockchain node) nil)

        (setf (node-current-leader node) (node-pkey *top-node*))
        (setf (node-hold-off node)       nil)
        (setf (node-hold-off-timer node)  (let ((this-node node))
                                            (ac::make-timer
                                             (lambda ()
                                               (format *standard-output* "~% **** hold-off timer fired ****~%")
                                               (send (node-pkey this-node) :end-holdoff)))))
        (clrhash (node-blockchain-tbl node))
        (clrhash (node-mempool        node))
        (clrhash (node-utxo-table     node))
        ))

;; -------------------------------------------------------------------

(defun send-subs (node &rest msg)
  (iter-subs node (lambda (sub)
                    (apply 'send sub msg))))

(defun group-subs (node)
  (um:accum acc
    (iter-subs node #'acc)))

(defun send-real-nodes (&rest msg)
  (loop for ip in *real-nodes* do
        (apply 'send (node-pkey (gethash ip *ip-node-tbl*)) msg)))

(defmethod short-id ((node node))
  (node-ip node)
  ;; (short-id (node-pkey node))
  )

(defmethod short-id (x)
  (let* ((str (base58-str x))
         (len (length str)))
    (if (> len 20)
        (concatenate 'string (subseq str 0 10)
                     ".."
                     (subseq str (- len 10)))
      str)))

;; ------------------------------

(defun sub-signing (my-node consensus-stage blk seq-id timeout)
  (declare (ignore my-node))
  (=lambda (node)
    (let ((start  (get-universal-time)))
      (send node :signing
            :reply-to        (current-actor)
            :consensus-stage consensus-stage
            :blk             blk
            :seq             seq-id
            :timeout         timeout)
      (labels
          ((!dly ()
                 #+:LISPWORKS
                 (send *dly-instr* :incr
                       (/ (- (get-universal-time) start)
                          timeout)))

               (=return (val)
                 (!dly)
                 (become 'do-nothing) ;; stop responding to messages
                 (=values val)))
        (recv
          ((list* :signed sub-seq ans)
           (if (eql sub-seq seq-id)
               (=return ans)
             ;; else
             (retry-recv)))
          
          (_
           (retry-recv))
          
          :TIMEOUT timeout
          :ON-TIMEOUT
          (progn
            (pr (format nil "SubSigning timeout waiting for ~A"
                        (short-id node)))
            (=return nil))
          )))))

;; ------------------------------

(defun end-all-holdoffs ()
  (loop for node across *node-bit-tbl* do
        (send (node-pkey node) :end-holdoff)))

(defvar *use-gossip* t)

(defun gossip-neighborcast (my-node &rest msg)
  "Gossip-neighborcast - send message to all nodes. Just a stub for now."
  (loop for node across *node-bit-tbl* do
        (unless (eql node my-node)
          (apply 'send (node-pkey node) msg))))

(defmethod add-sigs ((sig1 null) sig2)
  sig2)

(defmethod add-sigs ((sig1 pbc:signature) (sig2 null))
  sig1)

(defmethod add-sigs ((sig1 pbc:signature) (sig2 pbc:signature))
  (change-class (pbc:add-pts sig1 sig2)
                'pbc:signature))

(defun bft-threshold (blk)
  (* 2/3 (length (block-witnesses blk))))

(=defun gossip-signing (my-node consensus-stage blk blk-hash  seq-id timeout)
  (let ((*current-node* my-node))
    (cond ((and *use-gossip*
                (int= (node-pkey my-node) *leader*))
           ;; we are leader node, so fire off gossip spray and await answers
           (let ((bft-thrsh (bft-threshold blk))
                 (start     nil)
                 (g-bits    0)
                 (g-sig     nil))
             
             (pr "Running Gossip Signing")
             (gossip-neighborcast my-node :signing
                                  :reply-to        (current-actor)
                                  :consensus-stage consensus-stage
                                  :blk             blk
                                  :seq             seq-id
                                  :timneout        timeout)
             (setf start (get-universal-time))
             
             (labels
                 ((=return (val)
                    (pr "Return from Gossip Signing")
                    (become 'do-nothing) ;; stop responding to messages
                    (=values val))
                  
                  (=finish ()
                    (=return (if g-sig
                                 (list g-sig g-bits)
                               (list nil 0))))

                  (adj-timeout ()
                    (let ((stop (get-universal-time)))
                      (decf timeout (- stop (shiftf start stop))))))
                  
               (recv
                 ((list :signed sub-seq sig bits)
                  (when (and (eql sub-seq seq-id)
                             sig
                             (zerop (logand g-bits bits)) ;; check for no intersection
                             (pbc:check-hash blk-hash sig (composite-pkey blk bits)))
                    (pr (hex bits))
                    (setf g-bits (logior g-bits bits)
                          g-sig  (add-sigs sig g-sig)))
                  (if (> (logcount g-bits) bft-thrsh)
                      (=finish)
                    ;; else
                    (progn
                      (adj-timeout)
                      (retry-recv))))
                 
                 (msg
                  (pr (format nil "Gossip-wait got unknown message: ~A" msg))
                  (adj-timeout)
                  (retry-recv))
                 
                 :TIMEOUT    timeout
                 :ON-TIMEOUT (=finish)
                 ))))
  
          (t 
           ;; else - not leader don't re-gossip request for signatures
           (=values nil))
          )))

;; -------------------------------------------------------
;; VALIDATE-COSI-MESSAGE -- this is the one you need to define for
;; each different type of Cosi network...

(defun check-byz-threshold (bits blk)
  (or *in-simulatinon-always-byz-ok*
      (> (logcount bits)
         (bft-threshold blk))))

(defun check-block-transactions-hash (blk)
  (hash= (block-merkle-root-hash blk) ;; check transaction hash against header
         (compute-merkle-root-hash
          (block-transactions blk))))

(defmethod add-pkeys ((pkey1 null) pkey2)
  pkey2)

(defmethod add-pkeys ((pkey1 pbc:public-key) (pkey2 null))
  pkey1)

(defmethod add-pkeys ((pkey1 pbc:public-key) (pkey2 pbc:public-key))
  (change-class (pbc:add-pts pkey1 pkey2)
                'pbc:public-key))

(defun composite-pkey (blk bits)
  ;; compute composite witness key sum
  (let ((wsum  nil))
    (loop for ix from 0
          for wkey in (coerce (block-witnesses blk) 'list)
          do
          (when (logbitp ix bits)
            (setf wsum (add-pkeys wsum wkey))))
    wsum))

(defun check-hash-multisig (hash sig bits blk)
  (and sig
       (check-byz-threshold bits blk)
       (pbc:check-hash hash sig (composite-pkey blk bits))))

(defun check-block-multisignature (blk)
  (let* ((bits  (block-signature-bitmap blk))
         (sig   (block-signature blk))
         (hash  (hash/256 (signature-hash-message blk))))
    (and (check-hash-multisig hash sig bits blk)
         (check-block-transactions-hash blk))
    ))

(defun validate-cosi-message (node consensus-stage blk)
  (ecase consensus-stage
    (:prepare
     ;; blk is a pending block
     ;; returns nil if invalid - should not sign
     (and (int= *leader* (block-leader-pkey blk))
          (check-block-transactions-hash blk)
          (let ((prevblk (first *blockchain*)))
            (or (null prevblk)
                (and (> (block-timestamp blk) (block-timestamp prevblk))
                     (hash= (block-prev-block-hash blk) (hash-block prevblk)))))
          (or (int= (node-pkey node) *leader*)
              (check-block-transactions blk))
          ))

    (:commit
     ;; message is a block with multisignature check signature for
     ;; validity and then sign to indicate we have seen and committed
     ;; block to blockchain. Return non-nil to indicate willingness to sign.
     (unwind-protect
         (when (and (int= *leader* (block-leader-pkey blk))
                    (check-block-multisignature blk))
           (push blk *blockchain*)
           (setf (gethash (cosi/proofs:hash-block blk) *blockchain-tbl*) blk)
           (cond
             ;; For new tx: ultra simple for now: there's no UTXO
             ;; database. Just remhash from the mempool all
             ;; transactions that made it into the block.
             (*newtx-p*
              (cosi/proofs/newtx:clear-transactions-in-block-from-mempool blk))
             (t
              ;; clear out *mempool* and spent utxos
              (replay-remove-txs-from-mempool blk)
              (sync-database)))
           t) ;; return true to validate
       ;; unwind
       (end-holdoff)))
    ))

;; ----------------------------------------------------------------------------

(defun node-cosi-signing (reply-to consensus-stage blk seq-id timeout)
  ;; Compute a collective BLS signature on the message. This process
  ;; is tree-recursivde.
  (let* ((node     *current-node*)
         (blk-hash (hash/256 (signature-hash-message blk)))
         (subs     (and (not *use-gossip*)
                        (remove-if 'node-bad (group-subs node)))))
    (pr (format nil "Node: ~A :Stage ~A" (short-id node) consensus-stage))
    (=bind (ans)
        (par
          (let ((*current-node* node))
            (=values 
             ;; Here is where we decide whether to lend our signature. But
             ;; even if we don't, we stil give others in the group a chance
             ;; to decide for themselves
             (if (validate-cosi-message node consensus-stage blk)
                 (progn
                   (ac:pr (format nil "Block validated ~A" (short-id node)))
                   (list (pbc:sign-hash blk-hash (node-skey node))
                         (ash 1 (position (node-pkey node) (block-witnesses blk)
                                          :test 'int=))))
               (progn
                 (ac:pr (format nil "Block not validated ~A" (short-id node)))
                 (list nil 0)))))

          ;; ... and here is where we have all the subnodes in our
          ;; group do the same, recursively down the Cosi tree.
          (let ((fn (sub-signing node consensus-stage blk seq-id timeout)))
            (pmapcar fn subs))

          ;; gossip-mode group
          (gossip-signing node
                          consensus-stage
                          blk
                          blk-hash
                          seq-id
                          timeout))
      
      (let ((*current-node* node))
        (pr ans)
        (destructuring-bind ((sig bits) r-lst g-ans) ans
          (labels ((fold-answer (sub resp)
                     (cond
                      ((null resp)
                       ;; no response from node, or bad subtree
                       (pr (format nil "No signing: ~A"
                                   (short-id sub)))
                       (mark-node-no-response node sub))
                      
                      (t
                       (destructuring-bind (sub-sig sub-bits) resp
                         (if (and sub-sig
                                  (zerop (logand sub-bits bits)) ;; check for no overlap
                                  (pbc:check-hash blk-hash sub-sig (composite-pkey blk sub-bits)))
                             (setf sig  (add-sigs sig sub-sig)
                                   bits (logior bits sub-bits))
                           ;; else
                           (mark-node-corrupted node sub))
                         ))
                      )))
            (mapc #'fold-answer subs r-lst) ;; gather results from subs
            (when g-ans
              (fold-answer node g-ans))
            (send reply-to :signed seq-id sig bits))
          )))))

;; -----------------------------------------------------------

(defun node-compute-cosi (reply-to consensus-stage blk timeout)
  ;; top-level entry for Cosi signature creation
  ;; assume for now that leader cannot be corrupted...
  (let* ((self (current-actor))
         (hash (hash/256 (signature-hash-message blk)))
         (sess (int hash)))
    (ac:self-call :signing
                  :reply-to        self
                  :consensus-stage consensus-stage
                  :blk             blk
                  :seq             sess
                  :timeout         timeout)
    (recv
      ((list :signed seq sig bits)
       (cond
        ((eql seq sess)
         (if (check-hash-multisig hash sig bits blk)
             ;; we completed successfully
             (reply reply-to
                    (list :signature sig bits))
           ;; bad signature
           (reply reply-to :corrupt-cosi-network)
           ))
        ;; ------------------------------------
        (t ;; seq mismatch
           ;; must have been a late arrival
           (pr :late-arrival)
           (retry-recv))
        )) ;; end of message pattern
      ;; ---------------------------------
      ;; ---------------------------------
      ;; :TIMEOUT 30
      ;; :ON-TIMEOUT (reply reply-to :timeout-cosi-network)
      )))

;; ------------------------------------------------------------------------------------------------

(defun leader-assemble-block (trns prepare-timeout commit-timeout)
  (send *dly-instr* :clr)
  (send *dly-instr* :pltwin :histo-4)
  (pr "Assemble new block")
  (let* ((node      *current-node*)
         (self      (current-actor))
         (new-block (cosi/proofs:create-block (first *blockchain*)
                                              *election-proof* *leader*
                                              (map 'vector 'node-pkey *node-bit-tbl*)
                                              trns)))
    (ac:self-call :cosi-sign-prepare
                  :reply-to  self
                  :blk       new-block
                  :timeout   prepare-timeout)
    (pr "Waiting for Cosi prepare")
    (recv
      ((list :answer (list :signature sig bits))
       (let ((*current-node* node))
         (update-block-signature new-block sig bits)
         ;; we now have a fully assembled block with
         ;; multisignature.
         ;;
         ;; At the end of the following COMMIT phase, all
         ;; witnesses will end their holdoffs, regardless of
         ;; verification outcome
         (ac:self-call :cosi-sign-commit
                       :reply-to  self
                       :blk       new-block
                       :timeout   commit-timeout)
         (pr "Waiting for Cosi commit")
         (recv
           ((list :answer (list* :signature _))
            (send *dly-instr* :plt)
            (send (node-pkey node) :block-finished))
           
           ((list :answer (list :corrupt-cosi-network))
            (pr "Corrupt Cosi network in COMMIT phase"))
           
           #|
           ((list :answer (list :timeout-cosi-network))
            (pr "Timeout waiting for commitment multisignature"))
           |#
           )))
      
      ((list :answer (list :corrupt-cosi-network))
       (pr "Corrupt Cosi network in PREPARE phase")
       (end-all-holdoffs))
      
      #|
      ((list :answer (list :timeout-cosi-network))
       (pr "Timeout waiting for prepare multisignature"))
      |#
      )))
    
(defun leader-exec (prepare-timeout commit-timeout)
  (let ((trns  (get-transactions-for-new-block)))
(format *standard-output* "~&leader sees transactions ~a~&" trns)
    (if trns
        (leader-assemble-block trns prepare-timeout commit-timeout)
      ;; else
      (end-all-holdoffs))))

;; -----------------------------------------------------------------------------------
;; Test block assembly and verification...

(defvar *trans1* nil)
(defvar *trans2* nil)
(defvar *genesis* nil)

(defun tst-blk ()
  (reset-system)
  (spawn
   (lambda ()
     (labels
         ((bcast-msg (&rest msg)
            (map nil (lambda (node)
                       (apply 'send (node-pkey node) msg))
                 *node-bit-tbl*))
          (send-tx-to-all (tx)
            (bcast-msg :new-transaction :trn tx))
          (send-genesis-to-all (utxo)
            (bcast-msg :genesis-utxo :utxo utxo))
          (become-witness ()
            (bcast-msg :become-witness)))
       
       (become-witness)
       ;; -------------------------------------------------------------
       ;; manufacture two transactions and send to all nodes
       (if *trans1*
           (progn
             (send-genesis-to-all *genesis*)
             (send-tx-to-all *trans1*)
             (send-tx-to-all *trans2*))
         ;; else
         (let* ((k     (pbc:make-key-pair :dave)) ;; genesis keying
                (pkey  (pbc:keying-triple-pkey k))
                (skey  (pbc:keying-triple-skey k))
                
                (km    (pbc:make-key-pair :mary)) ;; Mary keying
                (pkeym (pbc:keying-triple-pkey km))
                (skeym (pbc:keying-triple-skey km)))
           
           (pr "Construct Genesis transaction")
           (multiple-value-bind (utxog secrg)
               (make-cloaked-txout 1000 pkey)
             (declare (ignore secrg))
             (send-genesis-to-all (setf *genesis* utxog))

             (let* ((minfo (decrypt-txout-info utxog skey))
                    (trans (make-transaction :ins `((:kind :cloaked
                                                     :amount ,(txout-secr-amt minfo)
                                                     :gamma  ,(txout-secr-gamma minfo)
                                                     :pkey   ,pkey
                                                     :skey   ,skey))
                                             :outs `((:kind :cloaked
                                                      :amount 750
                                                      :pkey   ,pkeym)
                                                     (:kind :cloaked
                                                      :amount 240
                                                      :pkey   ,pkey))
                                             :fee 10)))
               
               ;; send TX to all nodes
               (send-tx-to-all (setf *trans1* trans))
               
               (pr "Find UTX for Mary")
               (let* ((utxm   (find-txout-for-pkey-hash (hash:hash/256 pkeym) trans))
                      (minfo  (decrypt-txout-info utxm skeym)))
                 
                 (pr "Construct 2nd transaction")
                 (let ((trans (make-transaction :ins `((:kind :cloaked
                                                        :amount ,(txout-secr-amt minfo)
                                                        :gamma  ,(txout-secr-gamma minfo)
                                                        :pkey   ,pkeym
                                                        :skey   ,skeym))
                                                :outs `((:kind :cloaked
                                                         :amount 250
                                                         :pkey   ,pkeym)
                                                        (:kind :cloaked
                                                         :amount 490
                                                         :pkey  ,pkey))
                                                :fee 10)))
                   ;; send TX to all nodes
                   (send-tx-to-all (setf *trans2* trans))
                   ))))))
       ;; ------------------------------------------------------------------------
       (sleep 10)
       (map nil (lambda (node)
                  (setf (node-current-leader node) (node-pkey *top-node*))
                  (send (node-pkey node) :answer
                        (format nil "Ready-to-run: ~A" (short-id node))))
            *node-bit-tbl*)
       (send *top-node* :become-leader)
       (send *top-node* :make-block)
       ))))

;; -------------------------------------------------------------
;; Test with uncloaked transactions...

(defun tst-ublk ()
  (reset-system)
  (spawn
   (lambda ()
     (labels
         ((bcast-msg (&rest msg)
            (map nil (lambda (node)
                       (apply 'send (node-pkey node) msg))
                 *node-bit-tbl*))
          (send-tx-to-all (tx)
            (bcast-msg :new-transaction :trn tx))
          (send-genesis-to-all (utxo)
            (bcast-msg :genesis-utxo :utxo utxo))
          (become-witness ()
            (bcast-msg :become-witness)))
       
       (become-witness)
       ;; -------------------------------------------------------------
       ;; manufacture two transactions and send to all nodes
       (let* ((k     (pbc:make-key-pair :dave)) ;; genesis keying
              (pkey  (pbc:keying-triple-pkey k))
              (skey  (pbc:keying-triple-skey k))
              
              (km    (pbc:make-key-pair :mary)) ;; Mary keying
              (pkeym (pbc:keying-triple-pkey km))
              (skeym (pbc:keying-triple-skey km)))
         
         (pr "Construct Genesis transaction")
         (multiple-value-bind (utxog secrg)
             (make-uncloaked-txout 1000 pkey)
           (declare (ignore secrg))
           (send-genesis-to-all utxog)
           
           (let* ((amt   (uncloaked-txout-amt utxog))
                  (gamma (uncloaked-txout-gamma utxog))
                  (trans (make-transaction :ins `((:kind :uncloaked
                                                   :amount ,amt
                                                   :gamma  ,gamma
                                                   :pkey   ,pkey
                                                   :skey   ,skey))
                                           :outs `((:kind :uncloaked
                                                    :amount 750
                                                    :pkey   ,pkeym)
                                                   (:kind :uncloaked
                                                    :amount 240
                                                    :pkey   ,pkey))
                                           :fee 10)))
             
             ;; send TX to all nodes
             (send-tx-to-all trans)
             
             (pr "Find UTX for Mary")
             (let* ((utxm   (find-txout-for-pkey-hash (hash:hash/256 pkeym) trans))
                    (amt    (uncloaked-txout-amt utxm))
                    (gamma  (uncloaked-txout-gamma utxm)))
               
               (pr "Construct 2nd transaction")
               (let ((trans (make-transaction :ins `((:kind :uncloaked
                                                      :amount ,amt
                                                      :gamma  ,gamma
                                                      :pkey   ,pkeym
                                                      :skey   ,skeym))
                                              :outs `((:kind :uncloaked
                                                       :amount 250
                                                       :pkey   ,pkeym)
                                                      (:kind :uncloaked
                                                       :amount 490
                                                       :pkey  ,pkey))
                                              :fee 10)))
                 ;; send TX to all nodes
                 (send-tx-to-all trans)
                 )))))
       ;; ------------------------------------------------------------------------
       (sleep 10)
       (map nil (lambda (node)
                  (setf (node-current-leader node) (node-pkey *top-node*))
                  (send (node-pkey node) :answer
                        (format nil "Ready-to-run: ~A" (short-id node))))
            *node-bit-tbl*)
       (send *top-node* :become-leader)
       (send *top-node* :make-block)
       ))))

;; -------------------------------------------------------------

(defvar *arroyo*     "10.0.1.2")
(defvar *dachshund*  "10.0.1.3")
(defvar *malachite*  "10.0.1.6")
(defvar *rambo*      "10.0.1.13")

(defmethod damage ((ip string) t/f)
  (damage (gethash ip *ip-node-tbl*) t/f))

(defmethod damage ((node node) t/f)
  (setf (node-byz node) t/f))

(defun init-sim ()
  (shutdown-server)
  (reconstruct-tree)
  (prog1
      (start-server)
    (reset-system)))
