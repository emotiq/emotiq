;; cos-construction.lisp -- Generate simulation trees of network nodes for Cosi
;;
;; DM/Emotiq 02/18
;; -------------------------------------------------------------------------
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

(declaim (optimize (debug 3)))

;; --------------------------------------------------------------------
;; Physical network

(defvar *my-node*    nil) ;; which node my machine is on

;; default-timeout-period needs to be made smarter, based on node height in tree
(defparameter *default-timeout-period*   ;; good for 1600 nodes on single machine
  #+:LISPWORKS   10
  #+:ALLEGRO     70
  #+:CLOZURE     70
  #-(or :LISPWORKS :ALLEGRO :CLOZURE)
  70)

;; ----------------------------------------------------------------------
;; Network Tree Nodes

(defvar *bins-per-node* 9) ;; prolly needs to be >3 for BFT

(defun make-subs ()
  (make-array *bins-per-node*
              :initial-element nil))

(defclass node (gossip:gossip-node)
  ((pkeyzkp  :accessor node-pkeyzkp  ;; public key + ZKP
             :initarg  :pkeyzkp)
   (skey     :accessor node-skey     ;; private key
             :initarg  :skey)
   (pkey     :accessor node-pkey     ;; cached ECC point for public key
             :initarg  :pkey)
   (short-pkey :accessor node-short-pkey ;; short/fast public key for Randhound
               :initarg :short-pkey)
   (short-skey :accessor node-short-skey ;; short/fast secret key for Randhound
               :initarg :short-skey)
   (parent   :accessor node-parent   ;; points to node of group parent
             :initarg  :parent
             :initform nil)
   (subs     :accessor node-subs     ;; list of group members beyond self
             :initarg  :subs
             :initform (make-subs))
   (bit      :accessor node-bit      ;; bit position in bitmap
             :initform 0)
   ;; -------------------------------------
   ;; pseudo-globals per node
   (blockchain     :accessor node-blockchain
                   :initform nil)
   (blockchain-tbl :accessor node-blockchain-tbl
                   :initform (make-hash-table
                              :test 'equalp))
   (mempool        :accessor  node-mempool
                   :initform  (make-hash-table
                               :test 'equalp))
   (utxo-table     :accessor  node-utxo-table
                   :initform  (make-hash-table
                               :test 'equalp))

   (current-leader :accessor node-current-leader
                   :initform nil) ;; holds pkey of current leader node
   (current-beacon :accessor node-current-beacon
                   :initform nil) ;; holds pkey of 1-shot beacon assignee

   ;; list of pkeys that have called for new election
   (election-calls :accessor node-election-calls
                   :initform nil)

   ;; local-epoch records the last election seed
   (local-epoch    :accessor node-local-epoch
                   :initform 0)

   ;; had-work if we were witness and was called to perform work on a block
   (had-work       :accessor node-had-work
                   :initform nil)
   
   (tx-changes     :accessor node-tx-changes
                   :initform nil)
   ;; -------------------------------------
   (real-ip  :accessor node-real-ip  ;; real node for this node
             :initarg  :real-ip)
   (byz      :accessor node-byz      ;; Byzantine misbehav type
             :initform nil)
   (bad      :accessor node-bad      ;; if true then node was corrupted
             :initform nil)
   (load     :accessor node-load     ;; cpu loading of group for this node
             :initform 1)
   (self     :accessor node-self     ;; ptr to Actor handler
             :accessor gossip:application-handler ;; accessor used by gossip system to find our Actor
             :initarg  :self)
   (rh-state :accessor node-rh-state
             :initform nil)          ;; ptr to Randhound state info
   ))

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

;; randhound items
(define-symbol-macro *rh-state*       (node-rh-state       *current-node*))

(defun add-to-blockchain (node blk)
  (with-current-node node
    (let ((hashID (hash-block blk)))
      (setf *blockchain* hashID
            (gethash (bev-vec hashID) *blockchain-tbl*) blk))))

;; -------------------------------------------------------

(defmethod node-bitmap ((node node))
  (ash 1 (node-bit node)))

(defmethod iteri-subs ((node node) fn)
  (loop for sub across (node-subs node)
        for ix from 0
        when sub
        do (funcall fn ix sub)))

(defmethod iter-subs ((node node) fn)
  (iteri-subs node (lambda (ix sub)
                     (declare (ignore ix))
                     (funcall fn sub))))

(defmethod set-node-load (node)
  (setf (node-load node)
        (1+ (loop for sub across (node-subs node)
                  when sub
                  sum  (node-load sub)))))

;; -------------------------------------------------------------------

(defun need-to-specify (&rest args)
  (declare (ignore args))
  (error "Need to specify..."))

;; new for Gossip support, and conceptually cleaner...
(defmethod initialize-instance :around ((node node) &key pkey skey &allow-other-keys)
  (setf (node-self node) (make-node-dispatcher node)
        *current-node*   node)
  ;; -------------------------------------------------
  ;; Set up some short keys for Randhound
  ;;
  ;; This should likely be changed for production. The code here is
  ;; good'nuff for simulation
  ;;
  (let ((short-keys (pbc:with-pairing :PAIRING-AR160
                      (pbc:make-key-pair (list :RANDHOUND skey)))
                    ;; will be the hash of skey
                    ))
    (setf (node-short-pkey node) (pbc:keying-triple-pkey short-keys)
          (node-short-skey node) (pbc:keying-triple-skey short-keys)))
  ;; -------------------------------------------------
  (call-next-method))

(defmethod initialize-instance :after ((node node) &key &allow-other-keys)
  (when (null (node-blockchain node))
    (add-to-blockchain node (emotiq/config:get-genesis-block))))

;; --------------------------------------------------------------

(defun gen-uuid-int ()
  (uuid:uuid-to-integer (uuid:make-v1-uuid)))

;; --------------------------------------------------------------

(defun latest-block ()
  (and *blockchain*
       (gethash (bev-vec *blockchain*) *blockchain-tbl*)))

(defun block-list (&optional (from *blockchain*))
  (um:accum acc
    (um:nlet-tail iter ((id from))
      (when id
        ;; terminate on null reference (from genesis block)
          (um:if-let (blk (gethash (bev-vec id) *blockchain-tbl*))
            (progn
              ;; or terminate when missing the block
              (acc blk)
              (iter (block-prev-block-hash blk)))
            (warn "Missing block ~A -- you might want to ask for a back-fill" (short-id id)))
          ))))

#|
Twist comments, regarding possible messsage "you might want to ask for a back-fill"


That error is present because I didn't know what else to do there. It needs something better, but I don't know your wallet node architecture. Presumably the user needs to be told that he can't obtain the blockchain at the moment. We do 3 tries at random, and each try has random seeking to some depth (8 levels?).

The "you may want to ask for a backfill" should happen when tracing the blockchain, discovers some missing blocks (a hole - a backpointer hash value with no corresponding record in the *BLOCKCHAIN-TBL* hash table.)

So it sounds like you either have invalid back hash pointers, or are comparing them incorrectly (like yesterday), or else there really is a hole in your blockchain records.

(my bet says an incorrect comparison, but I believe I always convert to INT for a hash table key...)
...
Yes, I wrote that code last week, on Thursday, as I recall. A very loooong day too.

That error is present because I didn't know what else to do there. It needs something better, but I don't know your wallet node architecture. Presumably the user needs to be told that he can't obtain the blockchain at the moment. We do 3 tries at random, and each try has random seeking to some depth (8 levels?).

The "you may want to ask for a backfill" should happen when tracing the blockchain, discovers some missing blocks (a hole - a backpointer hash value with no corresponding record in the *BLOCKCHAIN-TBL* hash table.)

So it sounds like you either have invalid back hash pointers, or are comparing them incorrectly (like yesterday), or else there really is a hole in your blockchain records.

(my bet says an incorrect comparison, but I believe I always convert to INT for a hash table key...)
...
Also, while I doubt you have hit this condition yet, it is possible for more than one node to believe they are elected Leader, and will try to assemble a block and get the other witnesses to approve it.

The hope here (hope is not a strategy!!) is that a BFT super-majority of nodes will ignore one of the faux Leaders and discard his request for approval.

But if our logic is twisted, then it might be possible to have some weird hash back pointers. Our code would have to be really badly hosed for that to happen. The backlinks really ought to point to a block that really is on the blockchain.

... but just sayin...
...
so maybe the block creator is using a different hash method than the witness comparators?
....
Just looking again at [...] WITH-BLOCK-LIST macro -- puts me in mind of something that Pascal Costanza and his student came up with - dyanamic function bindings - to complement dynamic data bindings.

I have a copy of the code around here somewhere. About 1 page long. Very clever, and might actually do a better job of what Mark is trying to accomplish here (better in the sense of fewer unintended consequences).
...
Looked at the code where that "you might want to..." message arises. I wrote it. It is keying the hash table, INT-wise, a hash that is stored as an INT in the block. But I overtly call INT on the hash value anyway, just in case.

And the blocks are stored, also using the INT of their hash, into that hash table, and uses the same Hash function as the creator to find its key for insertion.

So that leads me to believe that your comparisons (performed by the hash table itself) are really valid. And so maybe there really is a hole?
...
The block header fields used to derive the hash value should all be immutable slots and unchanged after forming the hash. If anything in those header slots changes after the fact, then you could fail to derive the same hash value.

But we are hashing the header of the previous block, already inserted into the blockchain, and so that block should be entirely immutable at that point.

You could insert a periodic scanner to check the hashes of blocks in the blockchain and halt if it detects a change. If somewhere an immutable block got mutated...

Or just insert a check on every call to (BLOCK-LIST).

...

(maphash 
       (lambda (k v) 
             (assert (pbc= k (hash-block v))))
       *blockchain-tbl*)

|#

(defmacro with-block-list ((blockchain-list) &body body)
  `(let ((block-list (symbol-function 'cosi-simgen:block-list)))
     (setf (symbol-function 'cosi-simgen:block-list)
           (lambda () ,blockchain-list))
     (multiple-value-prog1
         (progn ,@body)
       (setf (symbol-function 'cosi-simgen:block-list) block-list))))


