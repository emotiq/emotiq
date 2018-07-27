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
   (parent   :accessor node-parent   ;; points to node of group parent
             :initarg  :parent
             :initform nil)
   (subs     :accessor node-subs     ;; list of group members beyond self
             :initarg  :subs
             :initform (make-subs))
   (bit      :accessor node-bit      ;; bit position in bitmap
             :initform 0)
   (stake    :accessor node-stake
             :initarg  :stake
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
   ))

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

(defvar *current-node*  nil)  ;; for sim = current node running

(defun current-node ()
  *current-node*)

;; new for Gossip support, and conceptually cleaner...
(defmethod initialize-instance :around ((node node) &key &allow-other-keys)
  (setf (node-self node) (make-node-dispatcher node))
  (setf *current-node* node)
  (call-next-method))

(defmethod initialize-instance :after ((node node) &key &allow-other-keys)
  (when (null (node-blockchain node))
    (let ((genesis-block (emotiq/config:get-genesis-block)))
      (push genesis-block (node-blockchain node))
      (setf (gethash (cosi/proofs:hash-block genesis-block)
                     (node-blockchain-tbl node))
            genesis-block))))

;; --------------------------------------------------------------

(defun gen-uuid-int ()
  (uuid:uuid-to-integer (uuid:make-v1-uuid)))



