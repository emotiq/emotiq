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

   (hold-off       :accessor node-hold-off
                   :initform nil)
   (hold-off-timer :accessor node-hold-off-timer)

   (current-leader :accessor node-current-leader
                   :initform nil) ;; holds pkey of current leader node
   (current-beacon :accessor node-current-beacon
                   :initform nil)
   (election-calls :accessor node-election-calls
                   :initform nil) ;; list of pkeys that have called for new election
   ;; local-epoch records the last election seed
   (local-epoch    :accessor node-local-epoch
                   :initform 0)
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
             :accessor application-handler ;; accessor used by gossip system to find our Actor
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

(defun make-node (ipstr pkeyzkp parent)
  (let* ((cmpr-pkey (first pkeyzkp))
         (pval      (keyval cmpr-pkey))
         (node (make-instance 'node
                              :ip      ipstr
                              :skey    (gethash pval *pkey-skey-tbl*)
                              :pkey    cmpr-pkey
                              :pkeyzkp pkeyzkp
                              :parent  parent
                              :real-ip *comm-ip*
                              )))
    (setf (node-self node) (cosi-simgen::make-node-dispatcher node) ;(make-node-dispatcher node)
          (gethash ipstr *ip-node-tbl*)   node
          (gethash pval  *pkey-node-tbl*) node)))

(defun need-to-specify (&rest args)
  (declare (ignore args))
  (error "Need to specify..."))

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

;; -------------------------------------------------------------

;; the *NODE-BIT-TBL* is really an ordered vector of all nodes in the
;; tree, ordered by public key magnitude

(defvar *node-bit-tbl* #())

(defun assign-bits ()
  ;; assign bit positions to each node
  (let ((collected
         (um:accum acc
           (maphash (lambda (k node)
                      (declare (ignore k))
                      (acc node))
                    *ip-node-tbl*))))
    (setf collected (sort collected '<
                          :key (um:compose 'keyval 'node-pkey)))
    (loop for node in collected
          for ix from 0 do
          (setf (node-bit node) ix))
    (setf *node-bit-tbl*
          (coerce collected 'vector))
    ))

(defun init-mappings ()
  (setf *my-node* nil
        *top-node* nil
        *leader-node* (get-local-ipv4 "localhost")
        *real-nodes*  (mapcar 'cdr *local-nodes*))
  (clrhash *ip-node-tbl*)
  (clrhash *pkey-node-tbl*)
  (clrhash *pkey-skey-tbl*)
  (setf *node-bit-tbl* #()))

;; -------------------------------------------------------------------
;; Node construction

(defun partition (node ip-list &key (key 'identity))
  (let* ((bins  (make-subs))
         (nbins (length bins))
         (vnode (dotted-string-to-integer (node-ip node))))
    (mapc (lambda (ip-arg)
            (let* ((vip (dotted-string-to-integer (funcall key ip-arg)))
                   (ix  (mod (logxor vnode vip) nbins)))
              (push ip-arg (aref bins ix))))
          ip-list)
    (setf (node-subs node) bins)))

(defun inner-make-node-tree (ip ip-list &optional parent)
  (multiple-value-bind (ipstr pkeyzkp)
      (gen-node-id ip)
    (let ((node (make-node ipstr pkeyzkp parent)))
      (when ip-list
        (let ((bins (partition node ip-list
                               :key (lambda (ip-arg)
                                      (if (consp ip-arg)
                                          (car ip-arg)
                                        ip-arg)))))
          (iteri-subs node
                      (lambda (ix subs)
                        (setf (aref bins ix)
                              (inner-make-node-tree (car subs)
                                                    (cdr subs)
                                                    node))))
          (set-node-load node)))
      node)))

(defun make-node-tree (ip vlist)
  ;; main entry point - captures IPv4 of arg ip for use as real-ip in
  ;; succeeding nodes
  (multiple-value-bind (ipstr pkeyzp)
      (gen-node-id ip)
    (let ((*comm-ip*  ipstr))
      (inner-make-node-tree (list ipstr pkeyzp) vlist))))

;; --------------------------------------------------------------------
;; for visual debugging...

#+:LISPWORKS
(progn
  (defmethod children (x layout)
    nil)
  
  (defmethod children ((node node) layout)
    (remove nil (coerce (node-subs node) 'list)))

  (defun split-to-octets (val)
    (um:nlet-tail iter ((n   4)
                        (val val)
                        (lst nil))
      (if (zerop n)
          lst
        (iter (1- n) (ash val -8) (cons (ldb (byte 8 0) val) lst)))
      ))

  (defclass red-text (capi:item-pinboard-object)
    ())

  (defclass black-text (capi:item-pinboard-object)
    ())

  (defmethod capi:draw-pinboard-object :around (pinboard (self red-text) &key &allow-other-keys)
    (gp:with-graphics-state (pinboard
                             :foreground :red)
      (call-next-method)))
  
  (defmethod make-node-pane (graphics-port (node node))
    (declare (ignore graphics-port))
    (let ((txt (node-ip node)))
      (make-instance (if (node-realnode node)
                         'red-text
                       'black-text)
                     :text txt)))

  (defmethod view-tree ((tree node) &key (layout :left-right))
    (capi:contain
     (make-instance 'capi:graph-pane
                    :layout-function layout
                    :roots (list tree)
                    :node-pane-function 'make-node-pane
                    :children-function (lambda (node)
                                         (children node layout))
                    ))))

;; --------------------------------------------------------------------
;; Initial Tree Generation and Persistence

(defvar *default-data-file* (asdf:system-relative-pathname :cosi-bls "config/cosi-nodes.txt"))
(defvar *default-key-file*  (asdf:system-relative-pathname :cosi-bls "config/cosi-keying.txt"))

(defun generate-ip ()
  ;; generate a unique random IPv4 address
  (let ((ip (integer-to-dotted-string (random #.(expt 2 32)))))
    (if (gethash ip *ip-node-tbl*)
        (generate-ip) ;; should be unlikely, would be 50% at around 2^16 nodes
      (setf (gethash ip *ip-node-tbl*) ip))))

(defmethod pair-ip-pkey ((node node))
  ;; used during initial tree generation
  ;; we need numeric values of keys for store in file
  (list (node-ip node)
        (node-pkeyzkp node)))

(defmethod pair-ip-pkey ((ip string))
  ;; used during initial tree generation
  ;; we need numeric values of keys for store in file
  (pair-ip-pkey (gethash ip *ip-node-tbl*)))

(defun gen-main-tree (leader real-nodes grps)
  (let* ((trees     (mapcar 'make-node-tree real-nodes grps))
         (main-tree (find leader trees
                          :test 'string=
                          :key  'node-ip)))
    ;; attach the non-leader real nodes to the leader node
    (let ((all-but (remove main-tree trees)))
      (setf (node-subs main-tree) (concatenate 'vector
                                               (node-subs main-tree)
                                               (apply 'vector all-but)))
      (dolist (tree all-but)
        (setf (node-parent tree) main-tree)))
    main-tree))

;; --------------------------------------------------------------
;; Generate Tree / Keying and save to startup init files

(defun generate-tree (&key datafile keyfile (nodes 1000))
  (init-mappings)
  (let* ((leader     *leader-node*)
         (real-nodes  (remove-duplicates *real-nodes*
                                         :test 'string=)))
    
    ;; ensure leader is in the real-nodes collection
    (assert (member leader real-nodes :test 'string=))
    
    ;; pre-populate hash table with real-nodes
    (clrhash *ip-node-tbl*)
    (clrhash *pkey-node-tbl*)
    (clrhash *pkey-skey-tbl*)
    (dolist (ip real-nodes)
      (setf (gethash ip *ip-node-tbl*) ip))

    ;; build the trees
    (let* ((nreal       (length real-nodes))
           (nodes/grp   (ceiling nodes nreal))
           (grps        (loop repeat nreal collect
                              (loop repeat nodes/grp collect
                                    (generate-ip))))
           (main-tree   (gen-main-tree leader real-nodes grps)))

      ;; save nodes as a text file for later
      (ensure-directories-exist *default-key-file*  :verbose t)
      (ensure-directories-exist *default-data-file* :verbose t)
      
      (with-open-file (f (or datafile *default-data-file*)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :rename)
        (with-standard-io-syntax
          (let ((*print-readably*     t)
                (*print-right-margin* 128))
            (pprint `(:leader     ,leader
                      :real-nodes ,(mapcar 'pair-ip-pkey real-nodes)
                      :groups     ,(mapcar (lambda (grp)
                                             (mapcar 'pair-ip-pkey grp))
                                           grps))
                    f))))
      
      ;; write the pkey/skey associations
      (with-open-file (f (or keyfile *default-key-file*)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :rename)
        (let ((keys nil))
          (maphash (lambda (k v)
                     (push (cons k v) keys))
                   *pkey-skey-tbl*)
          (with-standard-io-syntax
            (pprint keys f))))
      (assign-bits)
      #+:LISPWORKS (view-tree main-tree)
      (setf *my-node*  main-tree
            *top-node* main-tree)
      )))

;; ---------------------------------------------------------------
;; Tree reconstruction from startup init files

(defun read-data-file (path)
  (with-open-file (f path
                     :direction :input)
    (read f)))
      
(defun reconstruct-tree (&key datafile keyfile)
  ;; read the keying file
  (init-mappings)
  (let* ((key-path  (or keyfile 
                     *default-key-file*))
         (keys       (read-data-file key-path))
         (data-path   (or datafile
                          *default-data-file*))
         (data        (read-data-file data-path))
         (leader      (getf data :leader))
         (real-nodes  (getf data :real-nodes))
         (grps        (getf data :groups)))
    
    ;; sanity checking
    (assert (member leader real-nodes
                    :test 'string=
                    :key  'car))
    (labels ((no-dups (lst)
               (dolist (ip lst)
                 (destructuring-bind (ipstr zkp) ip
                   (assert (null (gethash ipstr *ip-node-tbl*)))
                   (let ((pval (keyval (first zkp))))
                     (assert (null (gethash pval *pkey-node-tbl*)))
                     (assert (apply 'cosi-keying:validate-pkey zkp))
                     (setf (gethash ipstr *ip-node-tbl*)   ip
                           (gethash pval  *pkey-node-tbl*) ip)
                     )))))
      (clrhash *ip-node-tbl*)
      (clrhash *pkey-node-tbl*)
      (no-dups real-nodes)
      (mapc #'no-dups grps))
    
    ;; reconstruct keying info
    (clrhash *pkey-skey-tbl*)
    (mapc (lambda (pair)
            (destructuring-bind (k . v) pair
              ;; k is integer, compressed pkey ECC pt
              ;; v is skey secret key
              (setf (gethash k *pkey-skey-tbl*) v)))
          keys)
    
    ;; reconstruct the trees
    (let ((main-tree  (gen-main-tree leader real-nodes grps)))
      (assign-bits)
      #+:LISPWORKS (view-tree main-tree)
      (setf *top-node* main-tree
            *my-node*  main-tree)) ;; (gethash (get-my-ipv4) *ip-node-tbl*)
    ))

