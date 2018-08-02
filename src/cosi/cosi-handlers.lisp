(in-package :emotiq/cosi)


(defvar *cosi-prepare-timeout* 10
  "Timeout in seconds that the leader waits during prepare phase for sealing a block.")

(defvar *cosi-commit-timeout* 10
  "Timeout in seconds that the leader waits during commit phase for sealing a block.")

(defvar *cosi-max-wait-period* 30
  "Timeout in seconds that represents the longest witness idle period before setting up an emergency call-for-new-election.")
  

(defvar *max-transactions* 16
  "max number of transactions per block")

(defvar *in-simulatinon-always-byz-ok* nil
  "set to nil for non-sim mode, forces consensus")

(defvar *election-proof* nil) ;; NYI


;; Support macros... ensure Node context is preserved into continuation bodies.
(defmacro with-current-node (node &body body)
  `(let ((*current-node* ,node))
     ,@body))

(defmacro =node-bind (parms expr &body body)
  ;; set up continuation that preserves Node context
  (let ((g!node  (gensym)))
    `(let ((,g!node  (current-node)))
       (=bind ,parms
           ,expr
         (with-current-node ,g!node  ;; restore node context
           ,@body)))))

(defmacro node-schedule-after (timeout &body body)
  ;; Actor schedule-after with Node context preservation
  `(=node-bind ()
       (ac:schedule-timeout-action ,timeout
         (=values))
     ,@body))

;; -------------------------------------------------------


(defmethod node-dispatcher (msg-sym &key)
  (error "Unknown message: ~A~%Node: ~A" msg-sym (node:short-id (current-node))))

(defmethod node-dispatcher ((msg-sym (eql :become-leader)) &key)
  (emotiq/tracker:track :new-leader (current-node))
  (setf *tx-changes* (node:make-tx-changes)))

(defmethod node-dispatcher ((msg-sym (eql :become-witness)) &key)
  (let ((node (current-node))
        (epoch *local-epoch*))
    (emotiq/tracker:track :new-witness node)
    (setf *tx-changes* (node:make-tx-changes)
          *had-work-p* nil)
    (node-schedule-after *cosi-max-wait-period*
      (when (= *local-epoch* epoch) ;; no intervening election
        (unless *had-work-p*
          ;; if we had work during this epoch before this timeout,
          ;; then a call-for-new-election has either already been sent,
          ;; or else a setup-emergency-call has been performed.
          (done-with-duties))))))
               
(defmethod node-dispatcher ((msg-sym (eql :reset)) &key)
  (emotiq/tracker:track :reset)
  (reset-nodes))

(defmethod node-dispatcher ((msg-sym (eql :answer)) &rest args)
  (pr args))

(defmethod node-dispatcher ((msg-sym (eql :genesis-block)) &key blk)
  (pr "~A got genesis block" (node:short-id (current-node)))
  (unless blk   ; Why are we using optional args. ???  -mhd, 6/12/18
    (error "BLK is nil, can't continue."))
  (push blk *blockchain*)
  (setf (gethash (block:hash blk) *blocks*) blk))

(defmethod node-dispatcher ((msg-sym (eql :new-transaction)) &key txn)
  (txn:validate txn))

(defmethod node-dispatcher ((msg-sym (eql :make-block)) &key)
  (emotiq/tracker:track :make-block)
  (leader-exec *cosi-prepare-timeout* *cosi-commit-timeout*))

(defmethod node-dispatcher ((msg-sym (eql :cosi-sign-prepare)) &key reply-to blk timeout)
  (emotiq/tracker:track :prepare)
  (node-compute-cosi reply-to :prepare blk timeout))

(defmethod node-dispatcher ((msg-sym (eql :cosi-sign-commit)) &key reply-to blk timeout)
  (emotiq/tracker:track :commit)
  (node-compute-cosi reply-to :commit blk timeout))

(defmethod node-dispatcher ((msg-sym (eql :signing)) &key reply-to consensus-stage blk seq timeout)
  ;; witness nodes receive this message to participate in a multi-signing
  (setf *had-work-p* t)
  (node-cosi-signing reply-to consensus-stage blk seq timeout))

(defmethod node-dispatcher ((msg-sym (eql :block-finished)) &key)
  (emotiq/tracker:track :block-finished)
  (pr "Block committed to blockchain")
  (pr "Block signatures = ~D" (logcount (block:signature-bitmap (first *blockchain*))))
  (send-hold-election))

(defmethod node-dispatcher ((msg-sym (eql :gossip)) &rest msg)
  ;; trap errant :GOSSIP messages that arrive at the Cosi application layer
  ;; These should not be happening, just log the incident, and ignore.
  (pr "WARN - Unexpected GOSSIP message: ~S" msg))


(defun signature-hash-message (blk)
  (block:serialize-header blk))

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
  (not (node:byzantine-p node))) ;; for now... should look at node-byz to see how to mess it up

(defun mark-node-no-response (node sub)
  (declare (ignore node sub)) ;; for now...
  nil)

(defun mark-node-corrupted (node sub)
  (declare (ignore node)) ;; for now...
  (setf (node:corrupted-p sub) t)
  nil)

;; -------------------------------------------------------------------

(defun send-subs (node &rest msg)
  (node:iter-other-members node (lambda (sub) (apply #'send sub msg))))

(defun group-subs (node)
  (um:accum acc
    (node:iter-other-members node #'acc)))

(defun send-real-nodes (&rest msg)
  (dolist (ip *real-nodes*)
    (apply #'send (node:pkey (gethash ip *ip->node*)) msg)))


(defun sub-signing (my-node consensus-stage blk seq-id timeout)
  (declare (ignore my-node))
  (=lambda (node)
    (let ((start (get-universal-time)))
      #-:LISPWORKS (declare (ignore start))
      (send node :signing
            :reply-to        (current-actor)
            :consensus-stage consensus-stage
            :blk             blk
            :seq             seq-id
            :timeout         timeout)
      (labels ((!dly ()
                 #+:LISPWORKS
                 (send *dly-instr* :incr (/ (- (get-universal-time) start) timeout)))
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
           (pr "SubSigning timeout waiting for ~A" (node:short-id node))
           (=return nil)))))))

;; -----------------------------------------------------------

(defmethod add-sigs ((sig1 null) sig2)
  sig2)

(defmethod add-sigs ((sig1 pbc:signature) (sig2 null))
  sig1)

(defmethod add-sigs ((sig1 pbc:signature) (sig2 pbc:signature))
  (change-class (pbc:add-pts sig1 sig2) 'pbc:signature))

;; -------------------------------------------------------------

(defmethod bft-threshold ((n fixnum))
  ;; return threshold that must be equal or exceeded
  (declare (fixnum n))
  (- n (floor n 3)))

(defmethod bft-threshold ((witnesses sequence))
  (bft-threshold (length witnesses)))

(defmethod bft-threshold ((blk block:eblock))
  (bft-threshold (block:witnesses blk)))

;; -------------------------------------------------------------

(=defun gossip-signing (my-node consensus-stage blk blk-hash seq-id timeout)
  (with-current-node my-node
    (if (and *use-gossip-p* (vec-repr:int= (node:pkey my-node) *leader*))
        ;; we are leader node, so fire off gossip spray and await answers
        (let ((bft-thrsh (1- (bft-threshold blk))) ;; adj by 1 since leader is also witness
              (start nil)
              (g-sig nil)
              (g-bits 0))
             
          (pr "Running Gossip Signing, Node = ~A" (node:short-id my-node))

          (neighborcast my-node
                        :signing
                        :reply-to        (current-actor)
                        :consensus-stage consensus-stage
                        :blk             blk
                        :seq             seq-id
                        :timeout         timeout)
             
          (setf start (get-universal-time))
             
          (labels ((=return (val)
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
                       (pr "Got bits: ~A" (vec-repr:hex-str bits))
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
             :ON-TIMEOUT (=finish))))
        
        (=values nil)))) ;; else - not leader don't re-gossip request for signatures

;; -------------------------------------------------------
;; VALIDATE-COSI-MESSAGE -- this is the one you need to define for
;; each different type of Cosi network...

(defun check-byz-threshold (bits blk)
  (or *in-simulatinon-always-byz-ok*
      (>= (logcount bits)
          (bft-threshold blk))))


(defmethod add-pkeys ((pkey1 null) pkey2)
  pkey2)

(defmethod add-pkeys ((pkey1 pbc:public-key) (pkey2 null))
  pkey1)

(defmethod add-pkeys ((pkey1 pbc:public-key) (pkey2 pbc:public-key))
  (change-class (pbc:add-pts pkey1 pkey2) 'pbc:public-key))

(defun composite-pkey (blk bits)
  "compute composite witness key sum"
  (let ((wsum nil))
    (loop
       :for ix :from 0
       :for wkey :in (coerce (block:witnesses blk) 'list)
       :do (when (logbitp ix bits)
             (setf wsum (add-pkeys wsum wkey))))
    wsum))

(defun check-hash-multisig (hash sig bits blk)
  (and sig
       (check-byz-threshold bits blk)
       (pbc:check-hash hash sig (composite-pkey blk bits))))

(defun check-block-multisignature (blk)
  (let* ((bits (block:signature-bitmap blk))
         (sig  (block:signature blk))
         (hash (hash:hash/256 (signature-hash-message blk))))
    (and (check-hash-multisig hash sig bits blk)
         (block:check-transactions-hash blk))))

(defun call-for-punishment ()
  ;; a witness detected a problem with a block handed by the leader... hmmm...
  ;; for now, just call for new election
  (when *use-real-gossip-p*
    (call-for-new-election)))

(defun done-with-duties ()
  (when *use-real-gossip-p*
    (setup-emergency-call-for-new-election)))
  
(defun validate-cosi-message (node consensus-stage blk)
  (ecase consensus-stage
    (:prepare
     ;; blk is a pending block
     ;; returns nil if invalid - should not sign
     (or (and (vec-repr:int= *leader*
                             (block:leader-pkey blk))
              (block:check-transactions-hash blk)
              (let ((prevblk (first *blockchain*)))
                (or (null prevblk)
                    (and (> (block:timestamp blk) (block:timestamp prevblk))
                         (vec-repr:int= (block:prev-block-hash blk) (block:hash prevblk)))))
              (or (vec-repr:int= (node:pkey node) *leader*)
                  (block:check-transactions blk)))
         (progn   ;; else - failure case
           (call-for-punishment)
           nil))) ;; return nil for failure

    (:commit
     ;; message is a block with multisignature check signature for
     ;; validity and then sign to indicate we have seen and committed
     ;; block to blockchain. Return non-nil to indicate willingness to sign.
     (or (and (vec-repr:int= *leader* (block:leader-pkey blk))
              (check-block-multisignature blk)
              (progn
                (push blk *blockchain*)
                (setf (gethash (block:hash blk) *blocks*) blk)
                ;; For new tx: ultra simple for now: there's no UTXO
                ;; database. Just remhash from the mempool all
                ;; transactions that made it into the block.
                (mempool:clear-block-transactions blk)
                (done-with-duties)
                t)) ;; return truea to validate
         (progn     ;; else - failure case
           (call-for-punishment)
           nil))))) ;; return nil for failure

;; ----------------------------------------------------------------------------

(defun node-cosi-signing (reply-to consensus-stage blk seq-id timeout)
  ;; Compute a collective BLS signature on the message. This process
  ;; is tree-recursivde.
  (let* ((node     (current-node))
         (blk-hash (hash:hash/256 (signature-hash-message blk)))
         (subs     (and (not *use-gossip-p*)
                        (remove-if 'node-bad (group-subs node)))))
    (pr "Node: ~A :Stage ~A" (node:short-id node) consensus-stage)
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
                      (let ((witnesses (block:witnesses blk)))
                        (pr "Block validated ~A" (node:short-id node))
                        (pr "Block witnesses = ~A" witnesses)
                        (let ((pos (position (node:pkey node) witnesses
                                             :test 'vec-repr:int=)))
                          (when pos
                            (list (pbc:sign-hash blk-hash (node:skey node))
                                  (ash 1 pos))))))
                 (progn
                   (pr "Block not validated ~A" (node:short-id node))
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
                       (pr "No signing: ~A" (node:short-id sub))
                       (mark-node-no-response node sub))
                      
                      (t
                       (destructuring-bind (sub-sig sub-bits) resp
                         (if (and sub-sig
                                  (zerop (logand sub-bits bits)) ;; check for no overlap
                                  (pbc:check-hash blk-hash sub-sig (composite-pkey blk sub-bits)))
                             (setf sig  (add-sigs sig sub-sig)
                                   bits (logior bits sub-bits))
                             (mark-node-corrupted node sub)))))))
            (mapc #'fold-answer subs r-lst) ;; gather results from subs
            (when g-ans
              (fold-answer node g-ans))
            (send reply-to :signed seq-id sig bits))))))

;; -----------------------------------------------------------

(defun node-compute-cosi (reply-to consensus-stage blk timeout)
  ;; top-level entry for Cosi signature creation
  ;; assume for now that leader cannot be corrupted...
  (let* ((node (current-node))
         (self (current-actor))
         (hash (hash:hash/256 (signature-hash-message blk)))
         (sess (vec-repr:int hash)))
    (ac:self-call :signing
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
               (progn ;; we completed successfully
                 (pr "Forwarding multisig to leader")
                 (reply reply-to :signature sig bits))
               (reply reply-to :corrupt-cosi-network))) ;; bad signature
          (t ;; seq mismatch
           ;; must have been a late arrival
           (pr "late-arrival")
           (retry-recv))))))))

;; ------------------------------------------------------------------------------------------------

(defun leader-assemble-block (txns prepare-timeout commit-timeout)
  (send *dly-instr* :clr)
  (send *dly-instr* :pltwin :histo-4)
  (pr "Assemble new block")
  (let* ((node (current-node))
         (self (current-actor))
         (new-block (block:make-block (first *blockchain*)
                                      *election-proof* *leader*
                                      (map 'vector #'first (get-witness-list))
                                      txns)))
    (ac:self-call :cosi-sign-prepare
                  :reply-to  self
                  :blk       new-block
                  :timeout   prepare-timeout)
    (pr "Waiting for Cosi prepare")
    (recv
      ((list :answer :signature sig bits)
       (pr "Made it back from Cosi validate")
       (with-current-node node
         (block:update-signature! new-block sig bits)
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
            (send (node:pkey node) :block-finished))
           
           ((list :answer :corrupt-cosi-network)
            (pr "Corrupt Cosi network in COMMIT phase")))))
      
      ((list :answer :corrupt-cosi-network)
       (pr "Corrupt Cosi network in PREPARE phase")))))
    
(defun leader-exec (prepare-timeout commit-timeout)
  (let ((txns (block:new-transactions :max *max-transactions*)))
    (pr "Leader see transactions: ~a" txns)
    (if txns
        (leader-assemble-block txns prepare-timeout commit-timeout)
        (done-with-duties))))

;; -----------------------------------------------------------------
;; debug

(defun init-sim ()
  (reconstruct-tree)
  (reset-system))

(defun reset-system ()
  (send-real-nodes :reset))

(defun reset-nodes ()
  (loop
     :for node :across *bitpos->node* :do
       (setf (node:corrupted-p node) nil
             (node:blockchain node) nil
             (node:current-leader node) (node:pkey *top-node*))
       (clrhash (node:blocks node))
       (clrhash (node:mempool node))
       (clrhash (node:utxos node))))

