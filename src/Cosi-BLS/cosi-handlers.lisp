
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

;; -------------------------------------------------------

(defparameter *current-node*  nil)  ;; for sim = current node running



(defun node-dispatcher (node &rest msg)
  (let ((*current-node* node))
    (um:dcase msg
      ;; ----------------------------
      ;; user accessible entry points - directed to leader node
      
      (:cosi-sign-prepare (reply-to msg)
       (node-compute-cosi node reply-to :prepare msg))
      
      (:cosi-sign-commit (reply-to msg)
       (node-compute-cosi node reply-to :commit msg))
      
      (:cosi-sign (reply-to msg)
       (node-compute-cosi node reply-to :notary msg))
      
      (:new-transaction (msg)
       (node-check-transaction msg))
      
      (:validate (reply-to sig bits)
       (node-validate-cosi reply-to sig bits))
      
      (:public-key (reply-to)
       (reply reply-to :pkey+zkp (node-pkeyzkp node)))
      
      (:add/change-node (new-node-info)
       (node-insert-node node new-node-info))
      
      (:remove-node (node-ip)
       (node-remove-node node node-ip))
      
      (:election (new-leader-ip)
       (node-elect-new-leader new-leader-ip))
      
      ;; -------------------------------
      ;; internal comms between Cosi nodes
      
      (:signing (reply-to consensus-stage msg seq)
       (case consensus-stage
         (:notary 
          (node-cosi-notary-signing node reply-to
                                    consensus-stage msg seq))
         (otherwise
          (node-cosi-signing node reply-to
                             consensus-stage msg seq))
         ))
      
      ;; -----------------------------------
      ;; for sim and debug
      
      (:make-block ()
       (leader-exec node))

      (:genesis-utxo (utxo)
       (record-new-utxo (bev (txout-hashlock utxo))))
      
      (:answer (&rest msg)
       ;; for round-trip testing
       (ac:pr msg))
      
      (t (&rest msg)
         (error "Unknown message: ~A~%Node: ~A" msg (node-ip node)))
      )))
  
;; -------------------------------------------------------

(defun make-node-dispatcher (node)
  ;; use indirection to node-dispatcher for while we are debugging and
  ;; extending the dispatcher. Saves reconstructing the tree every
  ;; time the dispatching chanages.
  (setf (cosi-simgen::node-self node)
        (ac:make-actor
         ;; one of these closures is stored in the SELF slot of every node
         (lambda (&rest msg)
           (apply 'node-dispatcher node msg)))))

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

(defun node-elect-new-leader (new-leader-ip)
  (let ((new-top-node (gethash new-leader-ip *ip-node-tbl*)))
    ;; Maybe... ready for prime time?
    (cond ((null new-top-node)
           (error "Not a valid leader node: ~A" new-leader-ip))
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
           (notify-real-descendents new-top-node :election new-leader-ip))
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
  ;; info is (ipv4 pkeyzkp)
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

(defun node-insert-node (node new-node-info)
  (destructuring-bind (ipstr pkeyzkp) new-node-info
    (let ((new-node (gethash ipstr *ip-node-tbl*)))
      (if new-node ;; already present in tree?
          ;; maybe caller just wants to change keying
          ;; won't be able to sign unless it know skey
          (multiple-value-bind (pkey ok) (check-pkey pkeyzkp)
            (when ok
              (setf (node-pkeyzkp new-node)  pkeyzkp
                    (node-pkey new-node)     pkey ;; cache the decompressed key
                    (node-real-ip new-node)  ipstr)))
        ;; else - not already present
        (node-model-insert-node *top-node* new-node-info))))
  (notify-real-descendents node :insert-node new-node-info))

;; ---------------------------------------------------------

(defun node-model-remove-node (gone-node)
  (remhash (node-ip gone-node) *ip-node-tbl*)
  (let ((pcmpr (keyval (first (node-pkeyzkp gone-node)))))
    (remhash pcmpr *pkey-node-tbl*)
    (remhash pcmpr *pkey-skey-tbl*)))

(defun node-remove-node (node gone-node-ipv4)
  (let ((gone-node (gethash gone-node-ipv4 *ip-node-tbl*)))
    (when gone-node
      (node-model-remove-node gone-node)
      ;; must rebuild tree to absorb node's subnodes
      (node-model-rebuild-tree nil *top-node*
                               (all-nodes-except *top-node*))
      (when (eq node *top-node*)
        (notify-real-descendents node :remove-node gone-node-ipv4)))))
  
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
            (ret      (make-return-addr my-ip my-port)))
         (labels
             ((exit ()
                (become 'do-nothing)
                (unregister-return-addr ret)
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
            (ret      (make-return-addr my-ip my-port)))
       (labels
           ((exit ()
              (become 'do-nothing)
              (unregister-return-addr ret)
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

;; --------------------------------------------------------------------
;; define a blockchain block

(defstruct bc-block
  hash
  protocol-version
  epoch
  timestamp
  prev-block-hash
  election-proof
  leader-pkey
  witnesses
  transactions
  signature
  signature-pkey
  signature-bitmap)

(defvar *protocol-version* #x100)
(defvar *election-proof*   nil)
(defvar *leader*           nil)

(define-symbol-macro *blockchain*     (node-blockchain *current-node*))
(define-symbol-macro *blockchain-tbl* (node-blockchain-tbl *current-node*))
(define-symbol-macro *mempool*        (node-mempool *current-node*))
(define-symbol-macro *utxo-table*     (node-utxo-table *current-node*))

(defvar *max-transactions*  16)  ;; max nbr TX per block
(defvar *in-simulatinon-always-byz-ok* t) ;; set to nil for non-sim mode, forces consensus

(defun signature-hash-message (blk)
  (list
   (bc-block-protocol-version blk)
   (bc-block-epoch            blk)
   (bc-block-timestamp        blk)
   (bc-block-prev-block-hash  blk)
   (bc-block-election-proof   blk)
   (bc-block-leader-pkey      blk)
   (bc-block-witnesses        blk)
   (bc-block-transactions     blk)))

(defun compute-block-hash (blk)
  (apply 'hash/256
         (nconc (signature-hash-message blk)
                (list
                 (bc-block-signature        blk)
                 (bc-block-signature-pkey   blk)
                 (bc-block-signature-bitmap blk)))
         ))

(defun get-block-transactions (blk)
  "Right now it is a simple partially ordered list of transactions.
Later it may become an ADS structure"
  (bc-block-transactions blk))

;; --------------------------------------------------------------------

(defmethod node-check-transaction (msg)
  "Just ignore invalid messages"
  nil)

(defmethod node-check-transaction ((msg transaction))
  (check-transaction-math msg))

;; -------------------------------
;; testing-version transaction cache

(defmethod cache-transaction ((key bev) val)
  (setf (gethash (bev-vec key) *mempool*) val))

(defmethod remove-tx-from-mempool ((key bev))
  (remhash (bev-vec key) *mempool*))

(defmethod remove-tx-from-mempool ((tx transaction))
  (remove-tx-from-mempool (bev (hash/256 tx))))

(defun cleanup-mempool (txs)
  "Undo changes to mempool and utxo database resulting from these
transactions."
  (dolist (tx txs)
    (let ((key (bev (hash/256 tx))))
      (cache-transaction key tx))
    (unspend-utxos tx)))
      
;; -------------------------------
;; testing-version TXOUT log

(defmethod lookup-utxo  ((key bev))
  (gethash (bev-vec key) *utxo-table*))

(defmethod record-new-utxo ((key bev))
  "KEY is Hash(P,C) of TXOUT - record tentative TXOUT. Once finalized,
they will be added to utxo-table"
  (let ((vkey (bev-vec key)))
    (multiple-value-bind (x present-p)
        (gethash vkey *utxo-table*)
      (declare (ignore x))
      (when present-p
        (error "Shouldn't Happen: Effective Hash Collision!!!"))
      (setf (gethash vkey *utxo-table*) :spendable))))

(defmethod record-new-utxo ((txout txout))
  (record-new-utxo (bev (txout-hashlock txout))))


(defmethod record-utxo ((key bev) val)
  (setf (gethash (bev-vec key) *utxo-table*) val))


(defmethod remove-utxo ((key bev))
  (remhash (bev-vec key) *utxo-table*))

(defmethod remove-utxo ((txin txin))
  (remove-utxo (bev (txin-hashlock txin))))

(defmethod remove-utxo ((txout txout))
  (remove-utxo (bev (txout-hashlock txout))))


(defmethod unspend-utxos ((tx transaction))
  (dolist (txin (trans-txins tx))
    (let ((key (bev (txin-hashlock txin))))
      (when (eql tx (lookup-utxo key))
        (record-utxo key :spendable))))
  (dolist (txout (trans-txouts tx))
    (remove-utxo txout)))

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
;;;     (when (and (notany (lambda (txin)
;;;                        ;; can't be spending an output you are just
;;;                        ;; now creating
;;;                        (find txin txout-keys))
;;;                      (txin-keys tx))
;;;              ;; now do the math
;;;              (validate-transaction tx))
;;;         (cache-transaction key tx)
         (let ((na (notany (lambda (txin)
                        ;; can't be spending an output you are just
                        ;; now creating
                        (find txin txout-keys))
                      (txin-keys tx)))
               (vt (validate-transaction tx)))
           (if (and na vt)
               (cache-transaction key tx)
             (error (format nil "invalid txn na=~A and vt=~A" na vt))))))

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
  (print "Topological sorting")
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
             (let ((key (bev (txin-hashlock txin))))
               ;; if not in UTXO table as :SPENDABLE, then it is invalid
               (when (eq :spendable (lookup-utxo key))
                 (record-utxo key tx)))))
    (cond ((every #'txin-ok (trans-txins tx))
           (dolist (txout (trans-txouts tx))
             (record-new-utxo (bev (txout-hashlock txout))))
           t)
          
          (t
           ;; remove transaction from mempool
           (remove-tx-from-mempool tx)
           (unspend-utxos tx)
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
  (let ((tx-pairs (get-candidate-transactions)))
    (print "Trimming transactions")
    (multiple-value-bind (hd tl)
        (um:split *max-transactions* tx-pairs)
      (dolist (tx tl)
        ;; put these back in the pond for next round
        (unspend-utxos tx))
      ;; now hd represents the actual transactions going into the next block
      hd)))
      
;; ----------------------------------------------------------------------
;; Code run by Cosi block validators...

(defun #1=check-block-transactions (tlst)
  "TLST is list of transactions from current pending block. Return nil
if invalid block.

List of TX should already have been topologically sorted so that input
UTXO's were created in earlier transactions or earlier blocks in the
blockchain.

Check that there are no forward references in spend position, then
check that each TXIN and TXOUT is mathematically sound."
  (dolist (tx tlst)
    (dolist (txin (trans-txins tx))
      (let ((key  (bev (txin-hashlock txin))))
        (unless (eql :SPENDABLE (lookup-utxo key))
          (return-from #1# nil)) ;; caller must back out the changes made so far...
        (record-utxo key tx)))   ;; mark as spend in this TX
    ;; now check for valid transaction math
    (unless (check-transaction-math tx)
      (return-from #1# nil))
    ;; add TXOUTS to UTX table
    (dolist (txout (trans-txouts tx))
      (record-new-utxo txout)))
  t) ;; tell caller everything ok

;; --------------------------------------------------------------------
;; Message handlers for verifier nodes

(defun node-validate-cosi (reply-to sig bits)
  ;; toplevel entry for Cosi signature validation checking
  ;; first check for valid signature...
  (if (pbc:check-message sig)
      ;; we passed signature validation on composite signature
      ;; now verify the public keys making up that signature
      (let* ((pkeys (reduce (lambda (lst node)
                              ;; collect keys from bitmap indication
                              (if (and node
                                       (logbitp (node-bit node) bits))
                                  (cons (node-pkey node) lst)
                                lst))
                            *node-bit-tbl*
                            :initial-value nil))
             ;; compute composite public key
             (tkey  (reduce 'pbc:mul-pts pkeys)))
        (reply reply-to :validation
               ;; see that our computed composite key matches the
               ;; key used in the signature
               (= (vec-repr:int (pbc:signed-message-pkey sig))
                  (vec-repr:int tkey))))
    
    ;; else - we failed initial signature validation
    (reply reply-to :validation nil)))

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
                  :color :red))
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

(defun clear-bad ()
  (send-real-nodes :reset))

(defun reset-nodes ()
  (loop for node across *node-bit-tbl* do
        (setf (node-bad        node) nil
              (node-blockchain node) nil)
        (clrhash (node-blockchain-tbl node))
        (clrhash (node-mempool        node))
        (clrhash (node-utxo-table     node))
        ))

(defun reset-blockchain ()
  "For debug..."
  (setf *leader* (node-pkey *top-node*))
  (reset-nodes))
           
;; -------------------------------------------------------------------

(defun send-subs (node &rest msg)
  (iter-subs node (lambda (sub)
                    (apply 'send sub msg))))

(defun group-subs (node)
  (um:accum acc
    (iter-subs node #'acc)))

(defun send-real-nodes (&rest msg)
  (loop for ip in *real-nodes* do
        (apply 'send (gethash ip *ip-node-tbl*) msg)))

;; ------------------------------

(defun sub-signing (my-ip consensus-stage msg seq-id)
  (=lambda (node)
    (let ((start    (get-universal-time))
          (timeout  10
                    ;; (* (node-load node) *default-timeout-period*)
                    )
          (ret-addr (make-return-addr my-ip)))
      (send node :signing ret-addr consensus-stage msg seq-id)
      (labels
          ((!dly ()
                 #+:LISPWORKS
                 (send *dly-instr* :incr
                       (/ (- (get-universal-time) start)
                          timeout)))

               (=return (val)
                 (!dly)
                 (unregister-return-addr ret-addr)
                 (=values val))
               
               (wait ()
                 (recv
                   ((list* :signed sub-seq ans)
                    (if (eql sub-seq seq-id)
                        (=return ans)
                      ;; else
                      (wait)))

                   (_
                    (wait))
                   
                   :TIMEOUT timeout
                   :ON-TIMEOUT
                   (progn
                     (pr (format nil "SubSigning timeout waiting for ~A" (node-ip node)))
                     (=return nil))
                   )))
        (wait))
      )))

;; ------------------------------

(defun gossip-neighborcast (node &rest msg)
  "Gossip-neighborcast - send message to all nodes. Just a stub for now."
  (declare (ignore node msg))
  t)

(=defun gossip-signing (node my-ip consensus-stage msg seq-id)
  (cond ((int= (node-pkey node) *leader*)
         ;; we are leader node, so fire off gossip spray and await answers
         (let ((timeout  10
                         ;; (* (node-load node) *default-timeout-period*)
                         )
               (ret-addr (make-return-addr my-ip))
               (g-bits   0)
               (g-sig    nil))

           (print "Running Gossip Signing")
           (gossip-neighborcast node :signing ret-addr consensus-stage msg seq-id)

           (labels
               ((=return (val)
                  (unregister-return-addr ret-addr)
                  (print "Return from Gossip Signing")
                  (=values val))
                
                (wait ()
                  (recv
                    ((list :signed sub-seq sig bits)
                     (when (and (eql sub-seq seq-id)
                                sig
                                (pbc:check-message sig))
                       (setf g-bits (logior g-bits bits)
                             g-sig  (if g-sig
                                       (pbc:combine-signatures g-sig sig)
                                     sig)))
                     (wait))
                    
                    (_
                     (wait))
                    
                    :TIMEOUT    timeout
                    :ON-TIMEOUT (=return (list g-sig g-bits))
                    )))
             (wait))))

        (t 
         ;; else - not leader don't re-gossip request for signatures
         (=values nil))
        ))

;; -------------------------------------------------------
;; VALIDATE-COSI-MESSAGE -- this is the one you need to define for
;; each different type of Cosi network... For now, just act as notary
;; service - sign anything.

(defun notary-validate-cosi-message (node consensus-stage msg)
  (declare (ignore node consensus-stage msg))
  t)

;; ------------------------------------------------------------------------

(defun check-byz-threshold (bits blk)
  (or *in-simulatinon-always-byz-ok*
      (> (logcount bits)
         (* 2/3 (length (bc-block-witnesses blk))))))

(defun validate-cosi-message (node consensus-stage blk)
  (ecase consensus-stage
    (:prepare
     ;; blk is a pending block
     ;; returns nil if invalid - should not sign
     (or (int= (node-pkey node) *leader*)
         (let ((txs  (get-block-transactions blk)))
           (or (check-block-transactions txs)
               ;; back out changes to *utxo-table*
               (progn
                 (dolist (tx txs)
                   (unspend-utxos tx))
                 nil)))))

    (:commit
     ;; message is a block with multisignature check signature for
     ;; validity and then sign to indicate we have seen and committed
     ;; block to blockchain. Return non-nil to indicate willingness to sign.
     (let ((signed (check-byz-threshold (bc-block-signature-bitmap blk) blk)))
       (unless signed
         (cleanup-mempool (get-block-transactions blk)))
       (when (and signed
                  (or (int= (node-pkey node) *leader*)
                      (and (int= (bc-block-hash blk)
                                 (compute-block-hash blk))
                           (pbc:check-message (make-instance 'pbc:signed-message
                                                             :msg  (signature-hash-message  blk)
                                                             :pkey (bc-block-signature-pkey blk)
                                                             :sig  (bc-block-signature      blk)))
                           )))
         (push blk *blockchain*)
         (setf (gethash (bc-block-hash blk) *blockchain-tbl*) blk)
         ;; clear out *mempool* and spent utxos
         (dolist (tx (get-block-transactions blk))
           (remove-tx-from-mempool tx)
           (dolist (txin (trans-txins tx))
             (remove-utxo txin)))
         t ;; return true to validate
         )))
    ))

(defun node-cosi-signing (node reply-to consensus-stage blk seq-id)
  ;; Compute a collective BLS signature on the message. This process
  ;; is tree-recursivde.
  (let* ((subs (remove-if 'node-bad (group-subs node))))
    (=bind (ans)
        (par
          (let ((*current-node* node))
            (=values 
             ;; Here is where we decide whether to lend our signature. But
             ;; even if we don't, we stil give others in the group a chance
             ;; to decide for themselves
             (if (validate-cosi-message node consensus-stage blk)
                 (list (pbc:sign-message (signature-hash-message blk)
                                         (node-pkey node)
                                         (node-skey node))
                       (node-bitmap node))
               (list nil 0))))
          ;; ... and here is where we have all the subnodes in our
          ;; group do the same, recursively down the Cosi tree.
          (pmapcar (sub-signing (node-real-ip node)
                                consensus-stage
                                blk
                                seq-id)
                   subs)
          ;; gossip-mode group
          (gossip-signing node
                          (node-real-ip node)
                          consensus-stage
                          blk
                          seq-id))
      (let ((*current-node* node))
        (destructuring-bind ((sig bits) r-lst g-ans) ans
          (labels ((fold-answer (sub resp)
                     (cond
                      ((null resp)
                       ;; no response from node, or bad subtree
                       (pr (format nil "No signing: ~A" (node-ip sub)))
                       (mark-node-no-response node sub))
                      
                      (t
                       (destructuring-bind (sub-sig sub-bits) resp
                         (if (and sub-sig
                                  (pbc:check-message sub-sig))
                             (setf sig  (if sig
                                            (pbc:combine-signatures sig sub-sig)
                                          sub-sig)
                                   bits (logior bits sub-bits))
                           ;; else
                           (mark-node-corrupted node sub))
                         ))
                      )))
            (mapc #'fold-answer subs r-lst) ;; gather results from subs
            (fold-answer node g-ans)
            (send reply-to :signed seq-id sig bits))
          )))))

(defun node-cosi-notary-signing (node reply-to consensus-stage msg seq-id)
  "This code is for simple testing. It will disappear shortly. Don't
bother factoring it with NODE-COSI-SIGNING."
  ;; Compute a collective BLS signature on the message. This process
  ;; is tree-recursivde.
  (let* ((subs (remove-if 'node-bad (group-subs node))))
    (=bind (ans)
        (par
          (=values 
           ;; Here is where we decide whether to lend our signature. But
           ;; even if we don't, we stil give others in the group a chance
           ;; to decide for themselves
           (if (notary-validate-cosi-message node consensus-stage msg)
               (list (pbc:sign-message msg (node-pkey node) (node-skey node))
                     (node-bitmap node))
             (list nil 0)))
          (pmapcar (sub-signing (node-real-ip node)
                                consensus-stage
                                msg
                                seq-id)
                   subs))
      (destructuring-bind ((sig bits) r-lst) ans
        (labels ((fold-answer (sub resp)
                   (cond
                    ((null resp)
                     ;; no response from node, or bad subtree
                     (pr (format nil "No signing: ~A" (node-ip sub)))
                     (mark-node-no-response node sub))
                    
                    (t
                     (destructuring-bind (sub-sig sub-bits) resp
                       (if (and sub-sig
                                (pbc:check-message sub-sig))
                           (setf sig  (if sig
                                          (pbc:combine-signatures sig sub-sig)
                                        sub-sig)
                                 bits (logior bits sub-bits))
                         ;; else
                         (mark-node-corrupted node sub))
                       ))
                    )))
          (mapc #'fold-answer subs r-lst) ;; gather results from subs
          (send reply-to :signed seq-id sig bits))
        ))))

;; -----------------------------------------------------------

(defun node-compute-cosi (node reply-to consensus-stage msg)
  ;; top-level entry for Cosi signature creation
  ;; assume for now that leader cannot be corrupted...
  (let ((sess (gen-uuid-int)) ;; strictly increasing sequence of integers
        (self (current-actor)))
    (ac:self-call :signing self consensus-stage msg sess)
    (labels
        ((wait-signing ()
           (recv
             ((list :signed seq sig bits)
              (cond
               ((eql seq sess)
                (if (pbc:check-message sig)
                    ;; we completed successfully
                    (reply reply-to
                           (list :signature sig bits))
                  ;; bad signature
                  (reply reply-to :corrupt-cosi-network)
                  ))
               ;; ------------------------------------
               (t ;; seq mismatch
                  ;; must have been a late arrival
                  (wait-signing))
               )) ;; end of message pattern
             ;; ---------------------------------
             ((list :new-transaction msg)
              ;; allow processing of new transactions while we wait
              (let ((*current-node* node))
                (node-check-transaction msg)
                (wait-signing)))
             ;; ---------------------------------
             :TIMEOUT 30
             :ON-TIMEOUT (reply reply-to :timeout-cosi-network)
             )))
      (wait-signing)
      )))

;; ------------------------------------------------------------------------------------------------

(defvar *blocks* nil)

(defun leader-exec (node)
  (send *dly-instr* :clr)
  (send *dly-instr* :pltwin :histo-4)
  (print "Assemble new block")
  (let ((new-block (make-bc-block
                    :protocol-version *protocol-version*
                    :epoch            (if *blockchain*
                                          (1+ (bc-block-epoch (first *blockchain*)))
                                        0)
                    :timestamp        (uuid:make-v1-uuid)
                    :prev-block-hash  (and *blockchain*
                                           (bc-block-hash (first *blockchain*)))
                    :election-proof   (base58 *election-proof*)
                    :leader-pkey      (base58 *leader*)
                    :witnesses        (map 'vector (um:compose 'base58 'node-pkey) *node-bit-tbl*)
                    :transactions     (get-transactions-for-new-block)))
        (self      (current-actor)))
    (ac:self-call :cosi-sign-prepare self new-block)
    (print "Waiting for Cosi prepare")
    (labels
        ((wait-prep-signing ()
           (recv
             ((list :answer (list :signature sig bits))
              (let ((*current-node* node))
                (setf (bc-block-signature        new-block) (base58 (pbc:signed-message-sig  sig))
                      (bc-block-signature-pkey   new-block) (base58 (pbc:signed-message-pkey sig))
                      (bc-block-signature-bitmap new-block) (base58 bits)
                      (bc-block-hash             new-block) (base58 (compute-block-hash new-block)))
                (ac:self-call :cosi-sign-commit self new-block)
                (print "Waiting for Cosi commit")
                (labels ((wait-cmt-signing ()
                           (recv
                             ((list :answer (list :signature _ bits))
                              (send *dly-instr* :plt)
                              (cond ((check-byz-threshold bits new-block)
                                     #+(or)
                                     (inspect new-block)
                                     (push new-block *blocks*)
                                     (print (format nil "~A (top-node=~A) Block committed to chain, epoch = ~A"
                                                    (cosi-simgen::node-ip node)
                                                    (cosi-simgen::node-ip *top-node*)
                                                    (bc-block-epoch new-block))))
                                    
                                    (t
                                     (print "Failed to get sufficient signatures during commit phase"))
                                    ))
                             
                             ((list :answer (list :corrupt-cosi-network))
                              (print "Corrupt Cosi network"))
                             
                             ((list :answer (list :timeout-cosi-network))
                              (print "Timeout waiting for commitment multisignature"))
                             
                             ;; ---------------------------------
                             ((list :new-transaction msg)
                              ;; allow processing of new transactions while we wait
                              (let ((*current-node* node))
                                (node-check-transaction msg)
                                (wait-cmt-signing))
                              ))))
                  (wait-cmt-signing))))
             
             ((list :answer (list :corrupt-cosi-network))
              (print "Corrupt Cosi network"))
             
             ((list :answer (list :timeout-cosi-network))
              (print "Timeout waiting for prepare multisignature"))
             
             ;; ---------------------------------
             ((list :new-transaction msg)
              ;; allow processing of new transactions while we wait
              (let ((*current-node* node))
                (node-check-transaction msg)
                (wait-prep-signing)))
             )))
      (wait-prep-signing))))

;; -------------------------------------------------------------------------------------
#|
;; FOR TESTING!!!

(setup-server)

(set-executive-pool 1)

(setf *real-nodes* (list *leader-node*))

(setf *real-nodes* (remove "10.0.1.13" *real-nodes*
                           :test 'string-equal))

(generate-tree :nodes 100)

(reconstruct-tree)
|#

(defun tst ()
  (spawn
   (lambda ()
     (send *dly-instr* :clr)
     (send *dly-instr* :pltwin :histo-4)
     (let ((ret   (make-return-addr (node-real-ip *my-node*)))
           (start (get-universal-time)))
       (labels
           ((exit ()
              (unregister-return-addr ret)))
         (send *top-node* :cosi-sign ret "This is a test message!")
         (recv
           ((list :answer
                  (and msg
                       (list :signature sig bits)))
            (send *dly-instr* :plt)
            (ac:pr
             (format nil "Total Witnesses: ~D" (logcount bits))
             msg
             (format nil "Duration = ~A" (- (get-universal-time) start)))
            
            (send *my-node* :validate ret sig bits)
            (recv
              ((list :answer :validation t/f)
               (if t/f
                   (ac:pr :valid-signature)
                 (ac:pr :invalid-signature))
               (exit))
              
              (msg
               (error "ValHuh?: ~A" msg)
               (exit))
              ))
           
           (msg
            (error "Huh? ~A" msg)
            (exit))
           ))))))

;; -----------------------------------------------------------------------------------
;; Test block assembly and verification...

(defvar *trans1* nil)
(defvar *trans2* nil)
(defvar *genesis* nil)

; temporary work-around
;(defmethod send ((self actor) &rest msg)
;(defmethod send ((node cosi-simgen::node) &rest msg)
;  (error "wrong node type for send"))

(defun tst-blk ()
     (reset-blockchain)
     (labels
         ((send-tx-to-all (tx)
            #||#
            (map nil (lambda (node)
                       (send (cosi-simgen::node-self node) :new-transaction tx))
                 *node-bit-tbl*)
            #||#
            ;; (send *top-node* :new-transaction tx)
            )
          (send-genesis-to-all (utxo)
            (map nil (lambda (node)
                       (send (cosi-simgen::node-self node) :genesis-utxo utxo))
                 *node-bit-tbl*))
          )
       
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
           
           (print "Construct Genesis transaction")
           (multiple-value-bind (utxog secrg)
               (make-txout 1000 pkey)
             (declare (ignore secrg))
             (send-genesis-to-all (setf *genesis* utxog))

             (let ((minfo (decrypt-txout-info utxog skey)))
               (multiple-value-bind (utxin info)  ;; spend side
                   (make-txin (txout-secr-amt minfo) ;; spend side
                              (txout-secr-gamma minfo)
                              pkey skey)
                 
                 (multiple-value-bind (utxo1 secr1) ;; sends
                     (make-txout 750 pkeym)
                   (multiple-value-bind (utxo2 secr2)
                       (make-txout 240 pkey)
                     
                     (let ((trans (make-transaction `(,utxin) `(,info)
                                                    `(,utxo1 ,utxo2)
                                                    `(,secr1 ,secr2)
                                                    :fee 10)))
                       
                       ;; send TX to all nodes
                       (send-tx-to-all (setf *trans1* trans))
                       
                       (print "Find UTX for Mary")
                       (let* ((utxm   (find-txout-for-pkey-hash (hash:hash/256 pkeym) trans))
                              (minfo  (decrypt-txout-info utxm skeym)))
                         
                         (print "Construct 2nd transaction")
                         (multiple-value-bind (utxin info)  ;; spend side
                             (make-txin (txout-secr-amt minfo)
                                        (txout-secr-gamma minfo)
                                        pkeym skeym)
                           
                           (multiple-value-bind (utxo1 secr1) ;; sends
                               (make-txout 250 pkeym)
                             (multiple-value-bind (utxo2 secr2)
                                 (make-txout 490 pkey)
                               
                               (let ((trans (make-transaction `(,utxin) `(,info)
                                                              `(,utxo1 ,utxo2)
                                                              `(,secr1 ,secr2)
                                                              :fee 10)))
                                 ;; send TX to all nodes
                                 (send-tx-to-all (setf *trans2* trans))
                                 ))))))
                     )))))))
       ;; ------------------------------------------------------------------------
       
       (send (node-self *top-node*) :make-block)
       ))

;; -------------------------------------------------------------

#+(AND :COM.RAL :LISPWORKS)
(defvar *arroyo*     "10.0.1.2")
#+(AND :COM.RAL :LISPWORKS)
(defvar *dachshund*  "10.0.1.3")
#+(AND :COM.RAL :LISPWORKS)
(defvar *malachite*  "10.0.1.6")
#+(AND :COM.RAL :LISPWORKS)
(defvar *rambo*      "10.0.1.13")

(defmethod damage ((ip string) t/f)
  (damage (gethash ip *ip-node-tbl*) t/f))

(defmethod damage ((node node) t/f)
  (setf (node-byz node) t/f))

(defun init-sim ()
  (shutdown-server)
  (reconstruct-tree)
  (start-server))
