;; mvp-election-beacon -- Elections for blockchain on training wheels
;;
;; DM/Emotiq  06/18
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

;; ---------------------------------------------------------------------------
;; list of (pubkey stake) associations for all witness nodes

(defun get-witness-list ()
  (gossip:get-live-uids))

(defun witness-p (pkey)
  (find pkey (get-witness-list)
        :test 'int=))

;; ---------------------------------------------------------------------------------------
;; Stake-weighted elections

(defclass tree-node ()
  ((l   :reader tree-node-l
        :initarg :l
        :initform nil)
   (r   :reader tree-node-r
        :initarg :r
        :initform nil)
   (sum :reader tree-node-sum
        :initarg :sum))
  (:documentation "Election tree node"))

(defmethod tree-node-p ((x tree-node))
  t)

(defmethod tree-node-p (x)
  (declare (ignore x))
  nil)

(defmethod node-stake ((node tree-node))
  (tree-node-sum node))

(defmethod node-stake ((node cons))
  (second node))

;; a tree of stakes, cosi-simgen:nodes at the leaves (very bottom of tree)

(defun make-tree-node (pair)
  (destructuring-bind (l &optional r) pair
    (if r
        (make-instance 'tree-node
                       :l  l
                       :r  r
                       :sum (+ (node-stake l)
                               (node-stake r)))
      ;; else
      l)))

(defun hold-election (nfrac &optional (nodes (emotiq/config:get-stakes)))
  "Given a fraction (0 < nfrac < 1) arrange all stakeholders into a
binary decision tree, and determine the node which wins the election,
based on their relative stake"
  (emotiq/tracker:track :election)
  (let* ((tree    (car (um:nlet-tail iter ((nodes nodes)) ;; tail recursive, scheme-like, named let
                         (if (= 1 (length nodes))
                             nodes
                           (iter (mapcar 'make-tree-node (um:group nodes 2))))))))
    (when tree
      (um:nlet-tail iter ((tree  tree))                       ;; tail recursive, scheme-like, named let
        (if (tree-node-p tree)
            (let* ((l      (tree-node-l tree))
                   (r      (tree-node-r tree))
                   (lstake  (node-stake l))
                   (tstake  (node-stake tree)))
              (if (< (/ lstake tstake) nfrac)
                    (iter r)
                  (iter l)))
          ;; else - we have arrived at a winner
          (first tree)))))) ;; return pkey of winner

;; --------------------------------------------------------------------------

(defvar *election-central*
  ;; an actor to prevent SMP multiple mutation of seed. Could also use
  ;; a monitor.
  (ac:make-actor
   (let ((seed  nil)) ;; actually okay if never reset
     (lambda (&rest msg)
       (um:dcase msg

         (:next-after-reset (&optional reply-to)
          ;; use a coarse (1 hour) value that we would all likely
          ;; agree upon
          (setf seed (round (get-universal-time) 3600))
          (ac:self-call :next reply-to))
         
         (:next (&optional reply-to)
          (setf seed (hash/256 seed))
          (let ((ans (float (/ (int seed) #.(ash 1 256)) 1d0)))
            (when reply-to
              (send reply-to ans))
            ans))

         (:update-seed (pkey)
          ;; perturb our seed so we don't mindlessly repeat the same
          ;; seed sequence as some other node after a major reset
          (setf seed (hash/256 (uuid:make-v1-uuid) ;; varies with machine and time
                               pkey                ;; varies with node
                               seed)))             ;; based on last seed we generated

         )))))

(defmacro with-election-reply (reply-args msg &body body)
  ;; define convenient continuation interface for actors calling on
  ;; election services for some result
  `(=node-bind ,reply-args
       (send *election-central* ,msg (lambda (val)
                                           (=values val)))
     ,@body))

(defun update-election-seed (pkey)
  (send *election-central* :update-seed pkey))

;; -----------------------------------------
;; REPL interface...

(defun get-election-seed ()
  (ac:ask *election-central* :next))

(defun reset-and-get-election-seed ()
  (ac:ask *election-central* :next-after-reset))

(defun fire-election () ;; for REPL playing...
  (with-current-node *my-node*
    (send-hold-election)))

#|
(progn
  (setf *election-seed* nil)
  (let* ((arr (loop repeat 10000 collect (get-election-seed))))
    (plt:histogram 'histo arr :clear t)))

(=bind (seed)
    (send *election-central* :next (lambda (val)
                                     (=values val)))
  (pr seed))
 |#

;; ---------------------------------------------------------------------------

(defun make-signed-message (msg)
  (append msg (list :sig (pbc:sign-hash msg (node-skey (current-node))))))

;; ---------------------------------------------------------------------------

(defun make-election-message-skeleton (pkey seed)
  `(:hold-an-election
       :n      ,seed
       :beacon ,pkey))

(defun make-signed-election-message (pkey seed)
  (make-signed-message (make-election-message-skeleton pkey seed)))

(defun validate-election-message (n beacon sig)
  (and (or (and (null *beacon*)      ;; it would be nil at startup
                (witness-p beacon))  ;;   from someone we know?
           (int= beacon *beacon*))   ;; or from the beacon we know?
       (let ((skel (make-election-message-skeleton beacon n)))
         (pbc:check-hash (hash/256 skel) sig beacon)))) ;; not a forgery

(defun send-hold-election ()
  (with-election-reply (seed) :next
    (broadcast+me (make-signed-election-message (node-pkey (current-node))
                                                seed))))

;; --------------------------------------------------------------------------
;; Augment message handlers for election process

(defmethod node-dispatcher ((msg-sym (eql :hold-an-election)) &key n beacon sig)
  (when (validate-election-message n beacon sig)
    (run-election n)))

(defun run-special-election ()
  ;; try to resync on stalled system
  (with-election-reply (seed) :next-after-reset
    (run-election seed)))
   
(defvar *mvp-election-period*  20) ;; seconds between election rounds

(defun stake-for (pkey)
  (let ((pair (find pkey (emotiq/config:get-stakes)
                    :key  'first
                    :test 'int=)))
    (and pair
         (cadr pair))))

(defun run-election (n)
  ;; use the election value n (0 < n < 1) to decide on staked winner
  ;; to become new leader node. Second runner up becomes responsible
  ;; for spawning an election beacon (one-shot).
  ;;
  ;; So election beacon becomes a roving responsibility. If it gets
  ;; knocked out, then we fall back to early elections with BFT
  ;; consensus on decision to resync.
  (let ((self      (current-actor))
        (node      (current-node)))
    
    (with-accessors ((me  node-pkey)) node

      (update-election-seed me)

      (let* ((winner     (hold-election n))
             (new-beacon (hold-election n (remove winner (emotiq/config:get-stakes)
                                                  :key 'first
                                                  :test 'int=))))

        (setf *leader*           winner
              *beacon*           new-beacon
              *local-epoch*      n  ;; unlikely to repeat from election to election
              *election-calls*   nil) ;; reset list of callers for new election
        
        (pr "~A got :hold-an-election ~A" (short-id node) n)
        (pr "election results ~A (stake = ~A)"
                     (if (int= me winner) " *ME* " " not me ")
                     (stake-for winner))
        (pr "winner ~A me=~A"
                     (short-id winner)
                     (short-id me))

        (when (int= me new-beacon)
          ;; launch a beacon
          (node-schedule-after *mvp-election-period*
            (send-hold-election)))

        (cond ((int= me winner)
               ;; why not use ac:self-call?  A: Because doing it with
               ;; send, instead, allows queued up messages to be
               ;; handled first. Might be some transactions waiting to
               ;; enter the pool.
               (send self :become-leader)
               (send self :make-block))

              (t
               ;; ditto
               (send self :become-witness))

              )))))

;; ---------------------------------------------------------------
;; if we don't hold a new election before this timeout, established at
;; the end of commit phase, then call for a new election
(defvar *emergency-timeout*  20)

(defun setup-emergency-call-for-new-election ()
  ;; give the election beacon a chance to do its thing
  (let* ((old-epoch *local-epoch*))
    (node-schedule-after *emergency-timeout*
      ;; first time processing at startup
      ;; go gather keys and stakes.
      ;; *local-epoch* will also not have
      ;; changed
      (when (= *local-epoch* old-epoch) ;; anything changed?
        (call-for-new-election)))
    ))

;; ----------------------------------------------------------------
;; Startup Init Stuff...

(defmethod node-dispatcher ((msg-sym (eql :startup-system)) &key &allow-other-keys)
  (setup-emergency-call-for-new-election))

(defun startup-elections ()
  ;; call this from global init after all housekeeping
  (gossip:broadcast :startup-system
                    :graphID :UBER))

(defmethod gossip:make-node ((kind (eql :cosi)) &key pkey skey)
  (setf *my-node* (make-instance 'node
                                 :pkey  (pbc:public-key pkey)
                                 :skey  (pbc:secret-key skey)))
  (gossip:initialize-node *my-node*
                          :pkey pkey
                          :skey skey)
  *my-node*)

(defun gossip:cosi-loaded-p ()
  t)

;; -----------------------------------------------------------

(Defun make-call-for-election-message-skeleton (pkey epoch)
  `(:call-for-new-election
    :pkey  ,pkey
    :epoch ,epoch))

(defun make-signed-call-for-election-message (pkey epoch)
  (make-signed-message (make-call-for-election-message-skeleton pkey epoch)))

(defun validate-call-for-election-message (pkey epoch sig)
  (and (= epoch *local-epoch*)        ;; talking about current epoch? not late arrival?
       (witness-p pkey)               ;; from someone we know?
       (not (find pkey *election-calls*  ;; not a repeat call
                  :test 'int=))
       (let ((skel (make-call-for-election-message-skeleton pkey epoch)))
         (pbc:check-hash (hash/256 skel) sig pkey))  ;; not a forgery
       (push pkey *election-calls*)))
  
(defun call-for-new-election ()
  (pr "Node ~A Calling for New Election" (short-id (current-node)))
  (with-accessors ((pkey  node-pkey)) (current-node)
    (unless (find pkey *election-calls* ;; prevent repeated calls
                  :test 'int=)
      (push pkey *election-calls*) ;; need this or we'll fail with only 3 nodes...
      (gossip:broadcast (make-signed-call-for-election-message pkey *local-epoch*)
                        :graphID :UBER))))

(defmethod node-dispatcher ((msg-sym (eql :call-for-new-election)) &key pkey epoch sig)
  (when (and (validate-call-for-election-message pkey epoch sig) ;; valid call-for-election?
             (or (pr "Node ~A got call for new election" (short-id (current-node))) t)
             (>= (length *election-calls*)
                (bft-threshold (get-witness-list))))
    (run-special-election)))


