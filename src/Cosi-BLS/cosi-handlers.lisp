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

(defun check-transaction-math (tx)
  "TX is a transaction. Check that every TXIN and TXOUT has a valid
range proof, and that the sum of TXIN equals the sum of TXOUT. In any
event record the transaction in a cache log to speed up later block
validation. If transaction was valid record itself as the value
corresponding to its hash as key, otherwise record a nil.

Also record the TXOUTS as unspent.

Return nil if transaction is invalid."
  (let* ((key        (hash/256 tx))
         (txouts     (trans-txouts tx))
         (txout-keys (mapcar 'txout-hashlock txouts)))
    (multiple-value-bind (v present-p) (lookup-transaction key)
      (cond (present-p  v)  ;; return nil, or non-nil as valid
            (t 
             (let ((valid-p (and (notany (lambda (txin)
                                           ;; can't be spending an
                                           ;; output you are just now
                                           ;; creating
                                           (find (txin-hashlock txin) txout-keys
                                                 :test 'equalp))
                                         (trans-txins tx))
                                 ;; now do the math
                                 (validate-transaction tx)
                                 )))
               (cache-transaction key (and valid-p tx))
               (when valid-p
                 ;; only add UTXO's if the transaction was valid
                 (dolist (txout txouts)
                   (record-new-utx (txout-hashlock)))
                 t))) ;; indicate valid transaction
            ))))

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

(defvar *txout-log*  (make-hash-table
                      :test 'equalp))

(defun record-new-utx (key)
  "KEY is Hash(P,C) of TXOUT - record tentative TXOUT. Once finalized,
they will be added to utxo-table"
  (multiple-value-bind (x present-p)
      (gethash key *txout-log*)
    (declare (ignore x))
    (when present-p
      (error "Shouldn't Happen: Effective Hash Collision!!!"))
    (setf (gethash key *txout-log*) :spendable)))

;; -------------------------------------------------------------------

#|
(defun partial-order (t1 t2)
  (some (lambda (txin)
          (member txin (cadr t1)))
        (car t2)))

(sort '(((12 2) (10 11))
        ((3 4) (12 14))
        ((10 5) (15 16)))
      'partial-order)
|#

(defun partial-order (t1 t2)
  (labels ((in-keys (lst)
             (mapcar (um:compose 'int 'txin-hashlock) lst))
           (out-keys (lst)
             (mapcar (um:compose 'int 'txout-hashlock) lst)))
    (let ((txouts1 (out-keys (trans-txouts t1)))
          (txins2  (in-keys  (trans-txins  t2))))
      (some (lambda (txin)
              (member txin txouts1))
            txins2))))
    
(defun topo-sort (tlst)
  (let* ((lst     (sort (copy-list tlst) 'partial-order))
         (valid-p (um:nlet-tail iter ((lst lst))
                    (or (null (cdr lst)) ;; empty or singleton
                        (let* ((txins (trans-txins (car lst)))
                               (ins   (mapcar (um:compose 'int 'txin-hashlock) txins)))
                          (labels ((dependent-on (tx)
                                     (let* ((txouts (trans-txouts tx))
                                            (outs   (mapcar (um:compose 'int 'txout-hashlock))))
                                       (some (lambda (in)
                                             (member in outs))
                                             ins))))
                            (when (notany #'dependent-on lst)
                              (iter (cdr lst))))
                          )))))
    (values lst valid-p)))

(defun check-block-transactions (tlst)
  "TLST is list of transactions from current pending block. Return nil
if invalid block. Need to topologically sort transactions so that all
TXIN follow transaction which produced the spent TXOUT."
  (let ((tsorted (topo-sort tlst))
        (valid-p (um:nlet-tail iter ((ts      tlst)
                                     (valid-p t))
                   ;; doing it this way allows us to scan all transactions
                   ;; and accumulate the mempool transactions, even if the block
                   ;; gets marked invalid this time around.
                   (if (endp ts)
                       valid-p
                     (let ((tx (first ts)))
                       (iter (rest ts)
                             (and (check-transaction-math tx)
                                  ;; now we have checked the math on all
                                  ;; transactions, recorded the transactions in a
                                  ;; log, and have now seen all TXOUT and recorded
                                  ;; them in another log.
                                  ;;
                                  ;; So check TXINS to be sure no double spending,
                                  ;; and no spending of unseen TXOUTS
                                  (check-double-spend tx)
                                  valid-p))
                       )))))
    (unless valid-p
      ;; remove all TX that were invalid - valid one's might show up
      ;; again in another attempt to form a block. And remove all
      ;; TXOUT that were spawned by invalid TX.
      (let ((invalid-utxos nil))
        (dolist (tx tlst)
          (let ((key (hash/256 tx)))
            (unless (gethash key *trans-cache*) ;; remove TX if was invalid
              (remhash key *trans-cache*)
              (setf invalid-utxos (append (trans-txouts tx) invalid-utxos))
              (dolist (txin (trans-txins tx))
                (let* ((key      (txin-hashlock txin))
                       (spent-tx (gethash key *txout-log*)))
                  (when (eq spent-tx tx)
                    ;; reset to unspent state
                    (setf (gethash key *txout-log*) :spendable))
                  ))
              )))
        (um:nlet-tail iter ((inv-utxos invalid-utxos))
          (when inv-utxos
            (let* ((hd  (first inv-utxos))
                   (tl  (rest inv-utxos))
                   (key (txout-hashlock hd))
                   (tx  (gethash key *txout-log*)))
              (cond ((null tx)  ;; not present in txout-log
                     (iter tl)) ;; so we already have transaction's txouts

                    ((eq :spendable tx) ;; not yet spent?
                     (remhash key *txout-log*)
                     (iter tl))
                    
                    (t
                     ;; this was a transaction that spent the utxo.
                     ;; now it is a victim of a bad utxo and must be cancelled too.
                     (remhash key *txout-log*)
                     (let ((txkey (hash/256 tx)))
                       (if (remhash txkey *trans-cache*) ;; present in trans-cache?
                           (iter (append (trans-txouts tx) tl)) ;; add its txouts to the list
                         (iter tl)))) ;; else - was already removed from trans-cache
                    ))))
        ))
    ;; clean up txout-log to show only unpsent utxos
    (let ((del-keys nil))
      (maphash (lambda (k v)
                 (unless (eq v :spendable)
                   (push k del-keys)))
               *txout-log*)
      (dolist (del-key del-keys)
        (remhash del-key *txout-log*)))
    ;; return verdict
    valid-p))

(defun check-double-spend (tx)
  "TX is transaction in current pending block.  Check every TXIN to be
sure no double-spending, nor referencing unknown TXOUT. Return nil if
invalid TX."
  (labels ((txin-ok (txin)
             (let ((key (txin-hashlock txin)))
               (multiple-value-bind (v present-p)
                   (gethash key *txout-log*)
                 (when (and present-p
                            (eq v :spendable)
                            ;; not allowed to spend your own output
                            (not (find key (trans-txouts tx)
                                       :test 'equalp
                                       :key  'txout-hashlock)))
                   (setf (gethash key *txout-log*) tx)) ;; record where TXOUT was spent
                 ))))
    (let ((valid-p (every #'txin-ok (trans-txins tx))))
      (unless valid-p
        (discard-transaction tx))
      valid-p))) ;; return verdict
    
(defun discard-transaction (tx)      
  (let ((txkey (hash/256 tx))) ;; remove transaction from mempool
    (remhash txkey *trans-cache*))
  ;; undo any spends
  (dolist (txin (trans-txins tx))
    (let ((key (txin-hashlock txin)))
      (when (eq tx (gethash key *txout-log*))
        (setf (gethash key *txout-log*) :spendable))
      ))
  ;; remove its utxos from the log - possibly taking out other
  ;; transactions too
  (um:nlet-tail iter ((txouts (trans-txouts tx)))
    (when txouts
      (let* ((hd  (first txouts))
             (tl  (rest txouts))
             (key (txout-hashlock hd))
             (tx  (gethash key *txout-log*)))
        (cond ((null tx) ;; not present in the log
               (iter tl))
              
              ((eq tx :spendable)
               (remash key *txout-log*)
               (iter tl))

              (t
               ;; transaction has spend the utxo - a victim of this
               ;; bad transaction, so undo the spend
               (remhash key *txout-log*)
               (let ((txkey (hash/256 tx)))
                 (if (remhash txkey *trans-cache*)
                     (iter (append (trans-txouts tx) tl))
                   ;; else
                   (iter tl))))
              )))))

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

(defun signed-message (msg)
  (NYI "signed-message"))

(defun signed-bitmap (msg)
  (NYI "signed-bitmap"))

(defun block-hash (blk)
  (NYI "block-hash"))

(defun get-block-transactions (blk)
  (NYI "get-block-transactions"))

(defun validate-cosi-message (node consensus-stage msg)
  (declare (ignore node)) ;; for now, in sim as notary
  (ecase consensus-stage
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
       (let ((key (block-hash (pbc:signed-message-msg msg))))
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
