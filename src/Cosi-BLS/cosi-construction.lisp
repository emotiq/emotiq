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

(defvar *local-nodes*  '(("localhost"    . "127.0.0.1")))
#|
(defvar *local-nodes*  '(("Arroyo.local"    . "10.0.1.33")
                         ("Malachite.local" . "10.0.1.6")
                         ("Dachshund.local" . "10.0.1.3")
                         ("Rambo"           . "10.0.1.13")
                         ("ChromeKote.local" . "10.0.1.36")))
|#
(defun get-local-ipv4 (node-name)
  (cdr (assoc node-name *local-nodes*
              :test 'string-equal)))
  
(defun get-my-ipv4 ()
  (get-local-ipv4 (machine-instance)))

(defvar *real-nodes*  (mapcar 'cdr *local-nodes*))
(defvar *leader-node* (get-local-ipv4 "localhost"))
;;(defvar *leader-node* (get-local-ipv4 "ChromeKote.local"))

(defvar *top-node*   nil) ;; current leader node
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
  ((ip       :accessor node-ip       ;; IPv4 string for this node
             :initarg  :ip)
   (pkeyzkp  :accessor node-pkeyzkp  ;; public key + ZKP
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

(defmethod node-realnode ((node node))
  (string-equal (node-ip node) (node-real-ip node)))

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

;; --------------------------------------------------------------------
;; For now, 3 different ways to specify a node:
;;   1. node structure pointer
;;   2. IPv4 address (in dotted string notation)
;;   3. PKEY (compressed public key ECC point)

;; XREF from IPv4 address to Tree Node
(defvar *ip-node-tbl*   (make-hash-table :test 'equal)) ;; IPv4 string as key
(defvar *pkey-node-tbl* (make-hash-table))              ;; compressed ECC pt integer as key

(defvar *pkey-skey-tbl* (make-hash-table))              ;; commpressed ECC pt integer as key

;; -------------------------------------------------------------------

(defvar *comm-ip*  nil) ;; internal use only

(defun make-cosi-tree-node (ipstr pkeyzkp parent)
  (let* ((cmpr-pkey (first pkeyzkp))
         (pval      (keyval cmpr-pkey))
         (node (gossip:make-node :cosi
                              :skey    (gethash pval *pkey-skey-tbl*)
                              :pkey    cmpr-pkey
                              )))
    (setf (node-ip node) ipstr
          (node-pkeyzkp node) pkeyzkp
          (node-parent node) parent
          (node-real-ip node) *comm-ip*)

    (setf (gethash ipstr *ip-node-tbl*)   node
          (gethash pval  *pkey-node-tbl*) node)))

(defun need-to-specify (&rest args)
  (declare (ignore args))
  (error "Need to specify..."))

;; new for Gossip support, and conceptually cleaner...
(defmethod initialize-instance :around ((node node) &key &allow-other-keys)
  (setf (node-self node) (make-node-dispatcher node))
  (call-next-method))

(defmethod initialize-instance :after ((node node) &key &allow-other-keys)
  (when (null (node-blockchain node))
    (let ((genesis-block (emotiq/config:get-genesis-block)))
      (push genesis-block (node-blockchain node))
      (setf (gethash (cosi/proofs:hash-block genesis-block)
                     (node-blockchain-tbl node))
            genesis-block))))

;; --------------------------------------------------------------

(defun check-pkey (zkp)
  ;; verify pkey zkp, return decompressed pkey ECC point
  (destructuring-bind (pkey psig) zkp
    (values pkey
            (cosi-keying:validate-pkey pkey psig))))

#+:ALLEGRO
(defun allegro-dotted-to-integer (string)
  (multiple-value-bind (start end starts ends)
      (#~m/^([0-9]+).([0-9]+).([0-9]+).([0-9]+)$/ string)
    (declare (ignore start end))
    (reduce (lambda (ans pair)
              (destructuring-bind (start . end) pair
                (logior (ash ans 8)
                        (parse-integer string :start start :end end))))
            (nreverse (pairlis (coerce starts 'list)
                               (coerce ends   'list)))
            :initial-value 0)))

#+:ALLEGRO
(defun allegro-integer-to-dotted (val)
  (let ((parts (um:nlet-tail iter ((n   4)
                                   (pos 0)
                                   (ans nil))
                 (if (zerop n)
                     ans
                   (iter (1- n) (+ pos 8) (cons (ldb (byte 8 pos) val) ans)))
                 )))
    (format nil "~{~d~^.~}" parts)))

(defun dotted-string-to-integer (string)
  #+:LISPWORKS (comm:string-ip-address string)
  #+:OPENMCL   (ccl::dotted-to-ipaddr string)
  #+:ALLEGRO   (allegro-dotted-to-integer string)
  #-(or :LISPWORKS :ALLEGRO :CLOZURE)
  (usocket:host-byte-order string))

(defun integer-to-dotted-string (val)
  #+:LISPWORKS (comm:ip-address-string val)
  #+:OPENMCL (CCL::ipaddr-to-dotted val)
  #+:ALLEGRO (allegro-integer-to-dotted val)
  #-(or :LISPWORKS :ALLEGRO :CLOZURE)
  (usocket:hbo-to-dotted-quad val))

(defun keyval (key)
  (vec-repr:int key))

(defun gen-uuid-int ()
  (uuid:uuid-to-integer (uuid:make-v1-uuid)))

(defun gen-node-id (ip)
  (if (consp ip)
      (values-list ip)
    (with-accessors ((pkey  pbc:keying-triple-pkey)
                     (psig  pbc:keying-triple-sig)
                     (skey  pbc:keying-triple-skey))
        (pbc:make-key-pair ip)
      (setf (gethash (keyval pkey) *pkey-skey-tbl*) skey)
      (values ip
              (list pkey psig))
      )))

