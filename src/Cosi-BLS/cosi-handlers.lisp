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

(defun node-dispatcher (node &rest msg)
   (um:dcase msg
     ;; ----------------------------
     ;; user accessible entry points - directed to leader node
     
     (:cosi-sign-prepare (reply-to msg)
      (node-compute-cosi node reply-to :prepare msg))

     (:cosi-sign-commit (reply-to msg)
      (node-compute-cosi node reply-to :commit msg))

     (:new-transaction (reply-to msg)
      (node-check-transaction node reply-to msg))
     
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
      (node-cosi-signing node reply-to consensus-stage msg seq))

     ;; -----------------------------------
     ;; for sim and debug
     
     (:answer (&rest msg)
      ;; for round-trip testing
      (ac:pr msg))

     (:reset ()
      (node-reset-nodes node))
     
     (t (&rest msg)
        (error "Unknown message: ~A~%Node: ~A" msg (node-ip node)))
     ))

;; -------------------------------------------------------

(defun make-node-dispatcher (node)
  ;; use indirection to node-dispatcher for while we are debugging and
  ;; extending the dispatcher. Saves reconstructing the tree every
  ;; time the dispatching chanages.
  (ac:make-actor
   ;; one of these closures is stored in the SELF slot of every node
   (lambda (&rest msg)
     (apply 'node-dispatcher node msg))))

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

(defmethod node-check-transaction (node reply-to msg)
  "Just ignore invalid messages"
  nil)

(defmethod node-check-transaction (node reply-to (msg transaction))
  (check-transaction-math msg))

;; -------------------------------
;; testing-version transaction cache

(defvar *trans-cache*  (make-hash-table
                        :test 'equalp))

(defun cache-transaction (key val)
  (setf (gethash key *trans-cache*) val))

(defun lookup-transaction (key)
  (gethash key *trans-cache*))

;; -------------------------------
;; testing-version TXOUT log

(defvar *utxo-table*  (make-hash-table
                      :test 'equalp))

(defun record-new-utx (key)
  "KEY is Hash(P,C) of TXOUT - record tentative TXOUT. Once finalized,
they will be added to utxo-table"
  (multiple-value-bind (x present-p)
      (gethash key *utxo-table*)
    (declare (ignore x))
    (when present-p
      (error "Shouldn't Happen: Effective Hash Collision!!!"))
    (setf (gethash key *utxo-table*) :spendable)))

;; -------------------------------------------------------------------

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
  (let* ((key        (hash/256 tx))
         (txout-keys (txout-keys tx)))
    (when (and (notany (lambda (txin)
                         ;; can't be spending an output you are just
                         ;; now creating
                         (find txin txout-keys))
                       (txin-keys tx))
               ;; now do the math
               (validate-transaction tx))
      (cache-transaction key (list key tx)))
    ))     

;; --------------------------------------------------------------------

(defun partial-order (t1 t2)
  "Return T if inputs follow outputs"
  (let ((txouts1 (txout-keys t1))
        (txins2  (txin-keys  t2)))
    (some (lambda (txin)
            (member txin txouts1))
          txins2)))
    
(defun topo-sort (tlst)
  "Topological partial ordering of transactions. TXOUT generators need
to precede TXIN users. Once partially ordered outs --> ins, then scan
for forward references, and trim them out of the original list.
Obviously an invalid situation, but proceed ahead to scan for
otherwise valid transactions...

TLST is a list of pairs (k v) with k being the hash of the
transaction, and v being the transaction itself."
  (cond ((null tlst) nil)
        (t
         (um:accum acc
           (um:nlet-tail iter ((lst (sort (copy-list tlst)
                                          'partial-order
                                          :key 'cadr)))
             ;; we know we have a car element since we weeded out null
             ;; lists in first clause.
             (let ((hd  (car lst))
                   (tl  (cdr lst)))
               (when tl
                 (let ((txins (txin-keys (cadr hd))))
                   (labels ((dependent-on (tx)
                              (let ((txouts (txout-keys (cadr tx))))
                                (some (lambda (txin)
                                        (member txin txouts))
                                      txins))))
                     (when (notany #'dependent-on lst)
                       (acc hd))
                     (iter tl))))
               ))))
        ))

(defun get-candidate-transactions ()
  "Scan available TXs for numerically valid, spend-valid, and return
topo-sorted partial order"
  (let ((txs  nil))
    (maphash (lambda (k v)
               (push (list k v) txs))
             *trans-cache*)
    (let ((trimmed (topo-sort txs)))
      (dolist (tx (set-difference txs trimmed
                                  :key 'car))
        ;; remove invalid transactions whose inputs refer to future
        ;; outupts
        (remhash (car tx) *trans-cache*))
      ;; checking for double spending also creates additional UTXO's.
      (um:accum acc
        (dolist (tx trimmed)
          (when (check-double-spend tx)
            (acc tx))))
      )))
               
(defun check-double-spend (tx-pair)
  "TX is transaction in current pending block.  Check every TXIN to be
sure no double-spending, nor referencing unknown TXOUT. Return nil if
invalid TX."
  (destructuring-bind (txkey tx) tx-pair
    (labels ((txin-ok (txin)
               (let ((key (txin-hashlock txin)))
                 (when (eq :spendable (gethash key *utxo-table*))
                   (setf (gethash key *utxo-table*) tx-pair)))))
      (cond ((every #'txin-ok (trans-txins tx))
             (dolist (txout (trans-txouts tx))
               (record-new-utx (txout-hashlock txout)))
             t)
            
            (t
             ;; remove transaction from mempool
             (remhash txkey *trans-cache*)
             (dolist (txin (trans-txins tx))
               (let ((key (txin-hashlock txin)))
                 (when (eq tx-pair (gethash key *utxo-table*)) ;; unspend all
                   (setf (gethash key *utxo-table*) :spendable))
                 ))
             nil)
            ))))

;; ----------------------------------------------------------------------

(defun check-block-transactions (tlst)
  "TLST is list of transactions from current pending block. Return nil
if invalid block. Need to topologically sort transactions so that all
TXIN follow transaction which produced the spent TXOUT. Check for cycles."
  (multiple-value-bind (valid-p tlst trimmed) (topo-sort tlst)
    (setf valid-p (um:nlet-tail iter ((ts      trimmed)
                                      (valid-p valid-p))
                    ;; doing it this way allows us to scan all transactions
                    ;; and accumulate the mempool transactions, even if the block
                    ;; gets marked invalid this time around.
                    (if (endp ts)
                        valid-p
                      (let ((tx (first ts)))
                        (iter (rest ts)
                              (and (check-transaction-math tx)
                                   ;; Now we have checked the math on
                                   ;; all elements of the transaction,
                                   ;; and recorded the transaction in
                                   ;; a log.
                                   ;;
                                   ;; So check TXINS to be sure no
                                   ;; double spending, and no spending
                                   ;; of unseen TXOUTS. Then record
                                   ;; TXOUTS in the UTXO log.
                                   (check-double-spend tx)
                                   valid-p))
                        ))))
    (unless valid-p
      ;; remove all TX that were invalid - valid one's might show up
      ;; again in another attempt to form a block. 
      ;;
      ;; Even though we have a trimmed list of TX some of the original
      ;; TX might have been recorded as they arrived. So we need to
      ;; clean up the ones that haven't passed the dependency ordering check.
      (dolist (tx (set-difference tlst trimmed))
        (let ((key (hash/256 tx)))
          (remhash key *trans-cache*)))
      ;; Then among the trimmed TX we need to back out their spending,
      ;; and remove their TXOUT till next time around.
      ;;
      ;; Also, remove any invalid TX from *trans-cache* so won't be
      ;; picked up later.
      (dolist (tx trimmed)
        (let ((key (hash/256 tx)))
          (if (gethash key *trans-cache*) ;; non-nil means math was okay
              (progn
                ;; was ostensibly valid, so leave in the trans-cache
                ;; for next time around
                (dolist (txin (trans-txins tx))
                  (let ((key (txin-hashlock txin)))
                    (when (eq tx (gethash key *utxo-table*)) ;; spent by this TX?
                      (setf (gethash key *utxo-table*) :spendable)))) ;; unspend it
                (dolist (txout (trans-txouts tx)) ;; remove utxos created in this TX
                  (remhash (txout-hashlock txout) *utxo-table*)))
            ;; else - was invalid math, just remove it so doesn't get
            ;; picked up later
            (remhash key *trans-cache*))
          )))

    ;; clean up utxo-table to show only unpsent utxos
    (let ((del-keys nil))
      (maphash (lambda (k v)
                 (unless (eq v :spendable)
                   (push k del-keys)))
               *utxo-table*)
      (dolist (del-key del-keys)
        (remhash del-key *utxo-table*)))
    ;; return verdict
    valid-p))

(defun assemble-block (tx-pairs)
  (NYI "assemble-block"))

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

;; -----------------------

(defun clear-bad ()
  (send-real-nodes :reset))

(defun node-reset-nodes (node)
  (declare (ignore node))
  (loop for node across *node-bit-tbl* do
        (setf (node-bad node) nil)))

;; ---------------

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

;; -------------------------------------------------------
;; VALIDATE-COSI-MESSAGE -- this is the one you need to define for
;; each different type of Cosi network... For now, just act as notary
;; service - sign anything.

(defvar *byz-thresh*  0)  ;; established at bootstrap time - consensus threshold
(defvar *blockchain*  (make-hash-table
                       :test 'equalp))
(defvar *block*  nil)  ;; next block being assembled

(defun signed-message (msg)
  (NYI "signed-message"))

(defun signed-bitmap (msg)
  (NYI "signed-bitmap"))

(defun compute-block-hash (blk)
  (NYI "compute-block-hash"))

(defun get-block-transactions (blk)
  (NYI "get-block-transactions"))

(defun validate-cosi-message (node consensus-stage msg)
  (declare (ignore node)) ;; for now, in sim as notary
  (ecase consensus-stage
    (:pre-prepare
     ;; attempt to assemble a block, first filtering pending transactions for validity.
     (setf *block* (assemble-block (get-candidate-transactions))))
    
    (:prepare
     ;; msg is a pending block
     (let ((txs  (get-block-transactions msg)))
       (check-block-transactions txs))) ;; returns nil if invalid - should not sign

    (:commit
     ;; message is a block with multisignature check signature for
     ;; validity and then sign to indicate we have seen and committed
     ;; block to blockchain. Return non-nil to indicate willingness to sign.
     (when (and (pbc:check-message (signed-message msg))
                (>= (logcount (signed-bitmap msg)) *byz-thresh*))
       (let ((key (compute-block-hash (pbc:signed-message-msg msg))))
         (setf (gethash key *blockchain*) msg))))
    ))

(defun node-cosi-signing (node reply-to consensus-stage msg seq-id)
  ;; Compute a collective BLS signature on the message. This process
  ;; is tree-recursivde.
  (let* ((subs (remove-if 'node-bad (group-subs node))))
    (=bind (v-lst r-lst)
        (ac:par
          (=values 
           ;; Here is where we decide whether to lend our signature. But
           ;; even if we don't, we stil give others in the group a chance
           ;; to decide for themselves
           (if (validate-cosi-message node consensus-stage msg)
               (list (pbc:sign-message msg (node-pkey node) (node-skey node))
                     (node-bitmap node))
             (list nil 0)))
          (pmapcar (sub-signing (node-real-ip node)
                                consensus-stage
                                msg
                                seq-id)
                   subs))
      (destructuring-bind (sig bits) v-lst ;; from validation effort
        (labels ((fold-answer (sub resp)
                   (cond
                    ((null resp)
                     ;; no response from node, or bad subtree
                     (pr (format nil "No signing: ~A" sub))
                     (mark-node-no-response node sub))
                    
                    (t
                     (destructuring-bind (sub-sig sub-bits) resp
                       (if (pbc:check-message sub-sig)
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
  (declare (ignore node))
  (let ((sess (gen-uuid-int)) ;; strictly increasing sequence of integers
        (self (current-actor)))
    (ac:self-call :signing self consensus-stage msg sess)
    (labels
        ((unknown-message (msg)
           (error "Unknown message: ~A" msg))
         
         (wait-signing ()
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
             (msg ;; other messages during commitment phase
                  (unknown-message msg))
             )))
      (wait-signing)
      )))

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
  (start-server))
