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

(defvar *use-gossip*      t)     ;; use Gossip graphs instead of Cosi Trees

;; ---------------------------------------------------------------

(defun NYI (&rest args)
  (error "Not yet implemented: ~A" args))

(defvar *cosi-prepare-timeout* 10
  "Timeout in seconds that the leader waits during prepare phase for sealing a block.")

(defvar *cosi-commit-timeout* 10
  "Timeout in seconds that the leader waits during commit phase for sealing a block.")

(defvar *cosi-max-wait-period* 30
  "Timeout in seconds that represents the longest witness idle period before setting up an emergency call-for-new-election.")
  
;; -------------------------------------------------------

(defvar *current-node*  nil)  ;; for sim = current node running

(defun current-node ()
  *current-node*)

(defmacro with-current-node (node &body body)
  `(let ((*current-node* ,node))
     ,@body))

(define-symbol-macro *blockchain*     (node-blockchain     *current-node*))
(define-symbol-macro *blockchain-tbl* (node-blockchain-tbl *current-node*))
(define-symbol-macro *mempool*        (node-mempool        *current-node*))
(define-symbol-macro *utxo-table*     (node-utxo-table     *current-node*))
(define-symbol-macro *leader*         (node-current-leader *current-node*))
(define-symbol-macro *tx-changes*     (node-tx-changes     *current-node*))
(define-symbol-macro *had-work*       (node-had-work       *current-node*))

;; election related items
(define-symbol-macro *election-calls* (node-election-calls *current-node*))
(define-symbol-macro *beacon*         (node-current-beacon *current-node*))
(define-symbol-macro *local-epoch*    (node-local-epoch    *current-node*))

;; -------------------------------------------------------

(defstruct tx-changes
  ;; all lists contain key vectors
  tx-dels    ;; list of pending transation removals from mempool
  utxo-adds  ;; list of pending new utxos
  utxo-dels) ;; list of pending spent utxos

;; -------------------------------------------------------
;; Support macros... ensure Node context is preserved into continuation bodies.

(defmacro =node-bind (parms expr &body body)
  ;; set up continuation that preserves Node context
  (let ((g!node  (gensym)))
    `(let ((,g!node  (current-node)))
       (=bind ,parms
           ,expr
         (with-current-node ,g!node  ;; restore node context
           ,@body)))
    ))

(defmacro node-schedule-after (timeout &body body)
  ;; Actor schedule-after with Node context preservation
  `(=node-bind ()
       (ac:schedule-timeout-action ,timeout
         (=values))
     ,@body))

;; -------------------------------------------------------

(defgeneric node-dispatcher (msg-sym &key &allow-other-keys))

(defmethod node-dispatcher (msg-sym &key)
  (error "Unknown message: ~A~%Node: ~A" msg-sym (short-id (current-node))))

(defmethod node-dispatcher ((msg-sym (eql :become-leader)) &key)
  (emotiq/tracker:track :new-leader (current-node))
  (setf *tx-changes* (make-tx-changes)))

(defmethod node-dispatcher ((msg-sym (eql :become-witness)) &key)
  (let ((node       (current-node))
        (epoch      *local-epoch*))
    (emotiq/tracker:track :new-witness node)
    (setf *tx-changes* (make-tx-changes))
    
    (setf *had-work* nil)
    (node-schedule-after *cosi-max-wait-period*
      (when (= *local-epoch* epoch) ;; no intervening election
        (unless *had-work*
          ;; if we had work during this epoch before this timeout,
          ;; then a call-for-new-election has either already been sent,
          ;; or else a setup-emergency-call has been performed.
          (done-with-duties)
          )))
    ))
               
(defmethod node-dispatcher ((msg-sym (eql :reset)) &key)
  (emotiq/tracker:track :reset)
  (reset-from-on-high))

(defmethod node-dispatcher ((msg-sym (eql :answer)) &rest args)
  (ac:pr args))

(defmethod node-dispatcher ((msg-sym (eql :genesis-utxo)) &key utxo)
  ;; A Genesis UTXO comes only once, being broadcast to the witness
  ;; group by a distinguished member who creates it.
  ;;
  ;; We can use the receipt of the genesis UTXO to also start up our
  ;; internal election beacon in loose synchrony with other witnesses.
  ;;
  (pr "~A got genesis utxo" (short-id (current-node)))
  (really-record-new-utxo utxo))

;; for new transactions:  -mhd, 6/12/18
(defmethod node-dispatcher ((msg-sym (eql :genesis-block)) &key blk)
  (pr "~A got genesis block" (short-id (current-node)))
  (unless blk ; Why are we using optional args. ???  -mhd, 6/12/18
    (error "BLK is nil, can't continue."))
  (let ((hashID (int (hash-block blk))))
    (setf *blockchain* hashID
          (gethash hashID *blockchain-tbl*) blk)))

;; for new transactions:  -mhd, 6/12/18
(defmethod node-dispatcher ((msg-sym (eql :new-transaction-new)) &key trn)
  (cosi/proofs/newtx:validate-transaction trn))

(defmethod node-dispatcher ((msg-sym (eql :make-block)) &key)
  (emotiq/tracker:track :make-block)
  (leader-exec *cosi-prepare-timeout* *cosi-commit-timeout*))

(defmethod node-dispatcher ((msg-sym (eql :cosi-sign-prepare)) &key reply-to blk timeout)
  (emotiq/tracker:track :prepare)
  (node-compute-cosi reply-to :prepare blk timeout))

(defmethod node-dispatcher ((msg-sym (eql :cosi-sign-commit)) &key reply-to blk timeout)
  (emotiq/tracker:track :commit)
  (node-compute-cosi reply-to :commit blk timeout))

(defmethod node-dispatcher ((msg-sym (eql :new-transaction)) &key trn)
  (node-check-transaction trn))

(defmethod node-dispatcher ((msg-sym (eql :signing))
                            &key reply-to consensus-stage blk seq timeout)
  ;; witness nodes receive this message to participate in a multi-signing
  ;;;
  ;;; prophylactic so leader does not sign its own messages
  ;;; TODO determine whether this is no necessary
  (unless (int= (node-pkey (current-node))
                *leader*)
    (setf *had-work* t)
    (node-cosi-signing reply-to
                       consensus-stage blk seq timeout)))

(defmethod node-dispatcher ((msg-sym (eql :leader-signing))
                            &key reply-to consensus-stage blk seq timeout)
  ;; Leader node receives this message to start a Cosi multi-signing
  (setf *had-work* t)
  (node-cosi-signing reply-to
                     consensus-stage blk seq timeout))

(defmethod node-dispatcher ((msg-sym (eql :block-finished)) &key)
  (emotiq/tracker:track :block-finished)
  (pr "Block committed to blockchain")
  (pr "Block signatures = ~D" (logcount (block-signature-bitmap (latest-block))))
  (send-blockchain-comment) ;; b'cast to everyone
  (send-hold-election))     ;; b'cast to witness pool

(defmethod node-dispatcher ((msg-sym (eql :blockchain-head)) &key pkey hashID sig)
  (assert (integerp hashID))
  (unless (eql *blockchain* hashID)
    (pr "Missing block head ~A, requesting fill" hashID)
    (setf *blockchain* hashID)
    (spawn 'sync-blockchain (current-node) hashID)))


(defvar *max-query-depth*  8)

(defmethod node-dispatcher ((msg-sym (eql :request-block)) &key replyTo hashId depth)
  (let ((blk (gethash hashID *blockchain-tbl*)))
    (if blk
        (reply replyTo :block-you-requested hashID blk)
      (when (< depth *max-query-depth*)
        (ask-neighbor-node :request-block   ;; else forward to someone else
                           :replyTo replyTo
                           :hashID  hashID
                           :depth   (1+ depth))
        ))))

(defmethod node-dispatcher ((msg-sym (eql :gossip)) &rest msg)
  ;; trap errant :GOSSIP messages that arrive at the Cosi application layer
  ;; These should not be happening, just log the incident, and ignore.
  (pr "WARN - Unexpected GOSSIP message: ~S" msg))

;; ------------------------------------------------------------------------------------

(defun make-node-dispatcher (node)
  (let ((beh  (make-actor
               (lambda (&rest msg)
                 (with-current-node node
                   (apply 'node-dispatcher msg)))
               )))
    (make-actor
     (lambda (&rest msg)
       ;;; Source of majority of messages on screen makes it fairly verbose
       (pr "Cosi MSG: ~A" msg)
       (um:dcase msg
         (:actor-callback (aid &rest ans)
          (let ((actor (lookup-actor-for-aid aid)))
            (when actor
              (apply 'send actor ans))
            ))
         
          (t (&rest msg)
             (apply 'send beh msg))
          )))))

;; -------------------------------------------------------------------------------------
;; Blockchain Sync

(defun ask-neighbor-node (&rest msg)
  (let* ((my-pkey (node-pkey (current-node)))
         (nodes   (remove my-pkey  ;; avoid asking myself
                          (get-witness-list)
                          :test 'int=)))
    (gossip:singlecast msg (nth (random (length nodes)) nodes)
                       :graphID :uber
                       :startNodeID my-pkey)))

(defun validate-block-signature (blkID blk)
  (let ((bits  (block-signature-bitmap blk))
        (pkey  (composite-pkey blk bits)))
    (and (int= blkID (hash-block blk))            ;; is it really the block I think it is?
         (>= (logcount bits) (bft-threshold blk)) ;; does it have BFT thresh signatures?
         (int= pkey (composite-pkey blk bits))    ;; is the signature pkey properly formed?
         (pbc:check-hash blkid                    ;; does the signature check out?
                         (block-signature blk)
                         pkey)
         )))

(defun sync-blockchain (node hashID &optional (retries 1))
  (when hashID
    (with-current-node node
      (ask-neighbor-node :request-block :replyTo (current-actor) :hashID hashID :depth 1)
      (let ((start   (get-universal-time))
            (timeout 60))
        (labels ((retry-ask ()
                   (if (>= retries 3)
                       (pr "ERROR - No response to blockchain query for block ~A" hashID)
                     (sync-blockchain node hashID (1+ retries)))))
          (recv
            ((list :answer :block-you-requested blkID blk)
             (with-current-node node
               (cond ((and (int= blkID hashID)
                           (validate-block-signature blkID blk)) ;; a valid response?
                      (setf (gethash blkID *blockchain-tbl*) blk)
                      (um:when-let (newID (block-prev-block-hash blk)) ;; do I have predecessor?
                        (unless (gethash newID *blockchain-tbl*)
                          (sync-blockchain node newID))))  ;; if not, then ask for it
                     (t
                      ;; wasn't what I asked for, or invalid block response
                      (setf timeout (- (+ start 60) (get-universal-time)))
                      (if (plusp timeout)
                          (retry-recv) ;; maybe node I asked forwarded to another node?
                        (retry-ask)))
                     )))
            
            :TIMEOUT    timeout
            :ON-TIMEOUT (retry-ask)
            ))))))

;; --------------------------------------------------------------------

(defvar *election-proof*   nil) ;; NYI

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
  (declare (ignore msg))
  nil)

(defmethod node-check-transaction ((msg transaction))
  (when *newtx-p*                       ; sanity check
    (error "This method must not be called in new-transactions mode."))
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
                     (short-id (current-node))))
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
  (cosi/proofs/newtx:get-transactions-for-new-block
   :max *max-transactions*))
      
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
  (gossip:broadcast :reset-from-on-high
                    :graphID :UBER)
  (reset-from-on-high))

(defun reset-from-on-high ()
  (let ((node (current-node)))
    (setf (node-bad        node) nil
          (node-blockchain node) nil)
    
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

(defmethod short-id ((node node))
  (short-id (node-pkey node)))

(defmethod short-id (x)
  (let* ((str (hex-str x))
         (len (length str)))
    (if (> len 14)
        (concatenate 'string (subseq str 0 7)
                     ".."
                     (subseq str (- len 7)))
      str)))

(defun node-id-str (node)
  (short-id node))

(defmethod print-object ((node node) out-stream)
  (format out-stream "#<NODE ~A>" (short-id node)))

;; ------------------------------

(defun sub-signing (my-node consensus-stage blk seq-id timeout)
  (declare (ignore my-node))
  (=lambda (node)
    (let ((start  (get-universal-time)))
      #-:LISPWORKS (declare (ignore start))
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
            (pr "SubSigning timeout waiting for ~A"
                         (short-id node))
            (=return nil))
          )))))

;; -----------------------------------------------------------

(defvar *cosi-gossip-neighborhood-graph* nil) ;; T if neighborhood graph has been established

(defun ensure-cosi-gossip-neighborhood-graph (my-node)
  (declare (ignore my-node))
  (or *cosi-gossip-neighborhood-graph*
      (setf *cosi-gossip-neighborhood-graph*
            (or :UBER ;; for now while debugging
                (gossip:establish-broadcast-group
                 (get-witness-list)
                 :graphID :cosi)))
      ))

(defun gossip-neighborcast (my-node &rest msg)
  "Gossip-neighborcast - send message to all witness nodes."
  (gossip:broadcast msg
                    :style :neighborcast
                    :graphID (ensure-cosi-gossip-neighborhood-graph my-node)))


(defun broadcast+me (msg)
  "Always goes to all local real nodes automatically"
  (gossip:broadcast msg
                    :graphID :UBER))

;; -----------------------------------------------------------

(defmethod add-sigs ((sig1 null) sig2)
  sig2)

(defmethod add-sigs ((sig1 pbc:signature) (sig2 null))
  sig1)

(defmethod add-sigs ((sig1 pbc:signature) (sig2 pbc:signature))
  (change-class (pbc:add-pts sig1 sig2)
                'pbc:signature))

;; -------------------------------------------------------------

(defmethod bft-threshold ((n fixnum))
  ;; return threshold that must be equal or exceeded
  (declare (fixnum n))
  (- n (floor n 3)))

(defmethod bft-threshold ((witnesses sequence))
  (bft-threshold (length witnesses)))

(defmethod bft-threshold ((blk eblock))
  (bft-threshold (block-witnesses blk)))

;; -------------------------------------------------------------

(=defun gossip-signing (my-node consensus-stage blk blk-hash seq-id timeout)
  (with-current-node my-node
    (cond ((and *use-gossip*
                (int= (node-pkey my-node) *leader*))
           ;; we are leader node, so fire off gossip spray and await answers
           (let ((bft-thrsh (1- (bft-threshold blk))) ;; adj by 1 since leader is also witness
                 (start     nil)
                 (g-bits    0)
                 (g-sig     nil))
             
             (pr "Running Gossip Signing, Node = ~A" (short-id my-node))

             (gossip-neighborcast my-node :signing
                                  :reply-to        (current-actor)
                                  :consensus-stage consensus-stage
                                  :blk             blk
                                  :seq             seq-id
                                  :timeout         timeout)
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
                  (with-current-node my-node
                    (cond ((and (eql sub-seq seq-id)
                                sig
                                (zerop (logand g-bits bits)) ;; check for no intersection
                                (pbc:check-hash blk-hash sig (composite-pkey blk bits)))
                           (pr "Got bits: ~A" (hex-str bits))
                           (setf g-bits (logior g-bits bits)
                                 g-sig  (add-sigs sig g-sig))
                           (if (>= (logcount g-bits) bft-thrsh)
                               (=finish)
                             ;; else
                             (progn
                               (adj-timeout)
                               (retry-recv))))

                          (t
                           (adj-timeout)
                           (retry-recv)))))
                 
                 (msg
                  (pr "Gossip-wait got unknown message: ~A" msg)
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
      (>= (logcount bits)
          (bft-threshold blk))))

(defun check-block-transactions-hash (blk)
  (hash= (block-merkle-root-hash blk) ;; check transaction hash against header
         (compute-merkle-root-hash blk)))

(defmethod add-pkeys ((pkey1 null) pkey2)
  pkey2)

(defmethod add-pkeys ((pkey1 pbc:public-key) (pkey2 null))
  pkey1)

(defmethod add-pkeys (pkey1 pkey2)
  (%add-pkeys (pbc:public-key pkey2) (pbc:public-key pkey1)))

(defmethod %add-pkeys ((pkey1 pbc:public-key) (pkey2 pbc:public-key))
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

(defun call-for-punishment ()
  ;; a witness detected a problem with a block handed by the leader... hmmm...
  ;; for now, just call for new election
  (call-for-new-election))

(defun done-with-duties ()
  (setup-emergency-call-for-new-election))

;; ----------------------------------------------------------------------

(defun make-blockchain-comment-message-skeleton (pkey hashID)
  `(:blockchain-head
    :pkey   ,pkey
    :hashID ,hashID))

(defun make-signed-blockchain-comment-message (pkey hashID)
  (make-signed-message (make-blockchain-comment-message-skeleton pkey hashID)))

(defun send-blockchain-comment ()
  (pr "TEST - Blockchain = ~A" *blockchain*)
  (inspect *blockchain*)
  (assert (or (null *blockchain*)
              (integerp *blockchain*)))
  (gossip:broadcast (make-signed-blockchain-comment-message (node-pkey (current-node))
                                                            *blockchain*)
                    :graphID :UBER))

;; ----------------------------------------------------------------------

(defun validate-cosi-message (node consensus-stage blk)
  (ecase consensus-stage
    (:prepare
     ;; blk is a pending block
     ;; returns nil if invalid - should not sign
     (or
      (and (int= *leader* (block-leader-pkey blk))
           (check-block-transactions-hash blk)
           (let ((prevblk (latest-block)))
             (or (null prevblk)
                 (and (> (block-timestamp blk) (block-timestamp prevblk))
                      (hash= (block-prev-block-hash blk) (hash-block prevblk)))))
           (or (int= (node-pkey node) *leader*)
               (check-block-transactions blk)))
      ;; else - failure case
      (progn
        (call-for-punishment)
        nil))) ;; return nil for failure

    (:commit
     ;; message is a block with multisignature check signature for
     ;; validity and then sign to indicate we have seen and committed
     ;; block to blockchain. Return non-nil to indicate willingness to sign.
     (or
      (and (int= *leader* (block-leader-pkey blk))
           (check-block-multisignature blk)
           (let ((hashID (int (hash-block blk))))
             (setf *blockchain* hashID
                   (gethash hashID *blockchain-tbl*) blk)
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
             (done-with-duties)
             t)) ;; return true to validate
      ;; else - failure case
      (progn
        (call-for-punishment)
        nil))) ;; return nil for failure
    ))

;; ----------------------------------------------------------------------------

(defun node-cosi-signing (reply-to consensus-stage blk seq-id timeout)
  ;; Compute a collective BLS signature on the message. This process
  ;; is tree-recursivde.
  (let* ((node     (current-node))
         (blk-hash (hash/256 (signature-hash-message blk)))
         (subs     (and (not *use-gossip*)
                        (remove-if 'node-bad (group-subs node)))))
    (pr "Node: ~A :Stage ~A" (short-id node) consensus-stage)
    (=node-bind (ans)
        ;; ----------------------------------
        (par
          ;; parallel task #1
          (with-current-node node
            (=values 
             ;; Here is where we decide whether to lend our signature. But
             ;; even if we don't, we stil give others in the group a chance
             ;; to decide for themselves
             (or (and (validate-cosi-message node consensus-stage blk)
                      (progn
                        (pr "Block validated ~A" (short-id node))
                        (pr "Block witnesses = ~A" (block-witnesses blk))
                        (let ((pos (position (node-pkey node) (block-witnesses blk)
                                             :test 'int=)))
                          (when pos
                            (list (pbc:sign-hash blk-hash (node-skey node))
                                  (ash 1 pos))))))
                 (progn
                   (pr "Block not validated ~A" (short-id node))
                   (list nil 0)))))

          ;; parallel task #2
          ;; ... and here is where we have all the subnodes in our
          ;; group do the same, recursively down the Cosi tree.
          (let ((fn (sub-signing node consensus-stage blk seq-id timeout)))
            (pmapcar fn subs))

          ;; parallel task #3
          ;; gossip-mode group
          (gossip-signing node
                          consensus-stage
                          blk
                          blk-hash
                          seq-id
                          timeout))
        ;; ----------------------------------

        (pr "Answer from cosi-signing: ~A" ans)
        (destructuring-bind ((sig bits) r-lst g-ans) ans
          (labels ((fold-answer (sub resp)
                     (cond
                      ((null resp)
                       ;; no response from node, or bad subtree
                       (pr "No signing: ~A"
                                    (short-id sub))
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
          ))))

;; -----------------------------------------------------------

(defun node-compute-cosi (reply-to consensus-stage blk timeout)
  ;; top-level entry for Cosi signature creation
  ;; assume for now that leader cannot be corrupted...
  (let* ((node (current-node))
         (self (current-actor))
         (hash (hash/256 (signature-hash-message blk)))
         (sess (int hash)))
    (ac:self-call :leader-signing
                  :reply-to        self
                  :consensus-stage consensus-stage
                  :blk             blk
                  :seq             sess
                  :timeout         timeout)
    (recv
      ((list :signed seq sig bits)
       (with-current-node node
         (cond
          ((eql seq sess)
           (pr "Made it back from signing")
           (if (check-hash-multisig hash sig bits blk)
               ;; we completed successfully
               (progn
                 (pr "Forwarding multisig to leader")
                 (reply reply-to :signature sig bits))
             ;; bad signature
             (reply reply-to :corrupt-cosi-network)
             ))
          ;; ------------------------------------
          (t ;; seq mismatch
             ;; must have been a late arrival
             (pr "late-arrival")
             (retry-recv))
          ))) ;; end of message pattern
      )))

;; ------------------------------------------------------------------------------------------------

(defun leader-assemble-block (trns prepare-timeout commit-timeout)
  (send *dly-instr* :clr)
  (send *dly-instr* :pltwin :histo-4)
  (pr "Assemble new block")
  (let* ((node      (current-node))
         (self      (current-actor))
         (new-block (cosi/proofs:create-block (latest-block)
                                              *election-proof* *leader*
                                              (coerce (get-witness-list) 'vector)
                                              trns)))
    (ac:self-call :cosi-sign-prepare
                  :reply-to  self
                  :blk       new-block
                  :timeout   prepare-timeout)
    (pr "Waiting for Cosi prepare")
    (recv
      ((list :answer :signature sig bits)
       (pr "Made it back from Cosi validate")
       (with-current-node node
         (update-block-signature new-block sig bits)
         ;; we now have a fully assembled block with
         ;; multisignature.
         (ac:self-call :cosi-sign-commit
                       :reply-to  self
                       :blk       new-block
                       :timeout   commit-timeout)
         (pr "Waiting for Cosi commit")
         (recv
           ((list* :answer :signature _)
            (pr "Made it back from Cosi commit with good signature")
            (send *dly-instr* :plt)
            (send (node-pkey node) :block-finished))
           
           ((list :answer :corrupt-cosi-network)
            (pr "Corrupt Cosi network in COMMIT phase"))
           )))
      
      ((list :answer :corrupt-cosi-network)
       (pr "Corrupt Cosi network in PREPARE phase"))
      )))
    
(defun leader-exec (prepare-timeout commit-timeout)
  (let ((trns  (get-transactions-for-new-block)))
    (pr "Leader see transactions: ~a" trns)
    (if trns
        (leader-assemble-block trns prepare-timeout commit-timeout)
      ;; else
      (progn
        (send-blockchain-comment)
        (done-with-duties)))
    ))

;; -----------------------------------------------------------------

(defun init-sim ()
  (reset-system))
