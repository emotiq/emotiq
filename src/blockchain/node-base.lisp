(in-package :emotiq/node)

;; (defgeneric dispatch (msg-sym &key &allow-other-keys))
    

(defvar *current-node*  nil)  ;; for sim = current node running

(defun current-node ()
  *current-node*)

(define-symbol-macro *blockchain*  (blockchain     *current-node*))
(define-symbol-macro *blocks*      (blocks         *current-node*))
(define-symbol-macro *mempool*     (mempool        *current-node*))
(define-symbol-macro *utxos*       (utxos          *current-node*))
(define-symbol-macro *leader*      (current-leader *current-node*))
(define-symbol-macro *tx-changes*  (tx-changes     *current-node*))
(define-symbol-macro *had-work-p*  (had-work-p     *current-node*))

;; election related items
(define-symbol-macro *election-calls* (election-calls *current-node*))
(define-symbol-macro *beacon*         (current-beacon *current-node*))
(define-symbol-macro *local-epoch*    (local-epoch    *current-node*))


;; Physical network

(defvar *local-nodes*  '(("localhost"    . "127.0.0.1")))

(defun get-local-ipv4 (node-name)
  (assoc-value *local-nodes* node-name :test #'string-equal))
  
(defvar *real-nodes*  (mapcar #'cdr *local-nodes*))
(defvar *leader-node* (get-local-ipv4 "localhost"))

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

(defun make-other-members ()
  (make-array *bins-per-node* :initial-element nil))

(defclass node (gossip:gossip-node)
  ((ip                          ;; IPv4 string for this node
    :accessor ip       
    :initarg  :ip)
   (real-ip                     ;; real node for this node
    :accessor real-ip  
    :initarg  :real-ip)
   (pkey-zkp                    ;; public key + ZKP
    :accessor pkey-zkp
    :initarg  :pkey-zkp)
   (skey                        ;; private key
    :accessor skey     
    :initarg  :skey)
   (pkey                        ;; cached ECC point for public key
    :accessor pkey     
    :initarg  :pkey)
   (parent                      ;; points to node of group parent
    :accessor parent   
    :initarg  :parent
    :initform nil)
   (other-members               ;; list of group members beyond self
    :accessor other-members
    :initarg  :other-members
    :initform (make-other-members))
   (bitpos                      ;; bit position in bitmap
    :accessor bitpos   
    :initform 0)
   (stake
    :accessor stake
    :initarg  :stake
    :initform 0)
   ;; -------------------------------------
   ;; pseudo-globals per node
   (blockchain
    :accessor blockchain
    :initform nil)
   (blocks
    :accessor blocks
    :initform (make-hash-table :test #'equalp))
   (mempool
    :accessor mempool
    :initform (make-hash-table :test #'equalp))
   (utxos
    :accessor utxos
    :initform (make-hash-table :test #'equalp))
   (current-leader                ;; holds pkey of current leader node
    :accessor current-leader
    :initform nil) 
   (current-beacon                ;; holds pkey of 1-shot beacon assignee
    :accessor current-beacon
    :initform nil) 
   (election-calls                ;; list of pkeys that have called for new election
    :accessor election-calls
    :initform nil)
   (local-epoch                   ;; last election seed
    :accessor local-epoch
    :initform 0)
   (had-work-p                    ;; had-work if we were witness and was called
    :accessor had-work-p          ;; to perform work on a block
    :initform nil)
   (tx-changes
    :accessor tx-changes
    :initform nil)
   ;; -------------------------------------
   (byzantine-p                 ;; indicates byzantine misbehavior
    :accessor byzantine-p      
    :initform nil)
   (corrupted-p                 ;; if true then node was corrupted
    :accessor corrupted-p      
    :initform nil)
   (cpu-load                    ;; cpu loading of group for this node
    :accessor cpu-load     
    :initform 1)
   (self                        ;; ptr to Actor handler
    :accessor self     
    :accessor gossip:application-handler ;; accessor used by gossip system to find our Actor
    :initarg :self)))


;; all lists in slots contain key vectors
(defstruct tx-changes
  tx-dels    ;; list of pending transation removals from mempool
  utxo-adds  ;; list of pending new utxos
  utxo-dels) ;; list of pending spent utxos


(defmethod bitmap ((node node))
  (ash 1 (bitpos node)))

;; (defmethod realnode ((node node))
;;   (string-equal (ip node) (real-ip node)))

(defmethod iteri-other-members ((node node) fn)
  (loop
     :for m :across (other-members node)
     :for ix :from 0
     :when m
     :do (funcall fn ix m)))

(defmethod iter-other-members ((node node) fn)
  (iteri-other-members node (lambda (ix m)
                              (declare (ignore ix))
                              (funcall fn m))))

(defmethod set-cpu-load (node)
  (setf (cpu-load node)
        (1+ (loop
               :for m :across (other-members node)
               :when m
               :sum (cpu-load m)))))


(defmethod short-id ((node node))
  (short-id (pkey node)))

(defmethod short-id (x)
  (let* ((str (vec-repr:hex-str x))
         (len (length str)))
    (if (> len 14)
        (concatenate 'string (subseq str 0 7)
                     ".."
                     (subseq str (- len 7)))
        str)))

(defmethod print-object ((node node) out-stream)
  (format out-stream "#<NODE ~A>" (short-id node)))

;; --------------------------------------------------------------------
;; For now, 3 different ways to specify a node:
;;   1. node structure pointer
;;   2. IPv4 address (in dotted string notation)
;;   3. PKEY (compressed public key ECC point)

(defvar *ip->node* (make-hash-table :test #'equal)
  "XREF from IPv4 address (as string) to Tree Node")

(defvar *pkey->node* (make-hash-table)
  "XREF from PKEY (as compressed ECC pt integer) to Tree Node")

(defvar *pkey->skey* (make-hash-table)
  "PKEY (as compressed ECC pt integer) to SKEY mapping")

;; -------------------------------------------------------------------

#+nil
(progn
(defvar *comm-ip*  nil) ;; internal use only

(defun make-node (ipstr pkey-zkp parent)
  (let* ((cmpr-pkey (first pkey-zkp))
         (pval (keyval cmpr-pkey))
         (skey (gethash pval *pkey->skey*))
         (node (make-instance 'node
                              :ip ipstr :real-ip *comm-ip*
                              :pkey cmpr-pkey :skey skey :pkey-zkp pkey-zkp
                              :parent parent)))
    (setf (gethash ipstr *ip->node*) node
          (gethash pval *pkey->node*) node)))
)
;; --------------------------------------------------------------

;; (defun check-pkey (zkp)
;;   ;; verify pkey zkp, return decompressed pkey ECC point
;;   (destructuring-bind (pkey psig) zkp
;;     (values pkey
;;             (cosi-keying:validate-pkey pkey psig))))

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
      (with-accessors ((pkey pbc:keying-triple-pkey)
                       (psig pbc:keying-triple-sig)
                       (skey pbc:keying-triple-skey))
          (pbc:make-key-pair ip)
        (setf (gethash (keyval pkey) *pkey->skey*) skey)
        (values ip
                (list pkey psig)))))

;; -------------------------------------------------------------

(defvar *bitpos->node* #()
  "vector of all nodes in the tree ordered by public key magnitude")

(defun assign-bit-positions ()
  ;; assign bit positions to each node
  (let ((collected (coerce (hash-table-values *ip->node*) 'vector)))
    (setf collected (sort collected #'< :key (compose #'keyval #'pkey)))
    (loop
       :for node :across collected
       :for ix :from 0
       :do (setf (bitpos node) ix))
    (setf *bitpos->node* collected)))

(defun init-mappings ()
  (setf *my-node* nil
        *top-node* nil
        *leader-node* (get-local-ipv4 "localhost")
        *real-nodes*  (mapcar #'cdr *local-nodes*))
  (clrhash *ip->node*)
  (clrhash *pkey->node*)
  (clrhash *pkey->skey*)
  (setf *bitpos->node* #()))

;; -------------------------------------------------------------------
;; Node construction

#+nil
(progn
(defun partition (node ip-list &key (key #'identity))
  (let* ((bins (make-other-members))
         (nbins (length bins))
         (vnode (dotted-string-to-integer (ip node))))
    (mapc (lambda (ip-arg)
            (let* ((vip (dotted-string-to-integer (funcall key ip-arg)))
                   (ix  (mod (logxor vnode vip) nbins)))
              (push ip-arg (aref bins ix))))
          ip-list)
    (setf (other-members node) bins)))

(defun inner-make-node-tree (ip ip-list &optional parent)
  (multiple-value-bind (ipstr pkey-zkp)
      (gen-node-id ip)
    (let ((node (make-node ipstr pkey-zkp parent)))
      (when ip-list
        (let ((bins (partition node ip-list
                               :key (lambda (ip-arg)
                                      (if (consp ip-arg)
                                          (car ip-arg)
                                          ip-arg)))))
          (iteri-other-members node
                               (lambda (ix members)
                                 (setf (aref bins ix)
                                       (inner-make-node-tree (car members)
                                                             (cdr members)
                                                             node))))
          (set-cpu-load node)))
      node)))

(defun make-node-tree (ip vlist)
  ;; main entry point - captures IPv4 of arg ip for use as real-ip in
  ;; succeeding nodes
  (multiple-value-bind (ipstr pkey-zkp)
      (gen-node-id ip)
    (let ((*comm-ip* ipstr))
      (inner-make-node-tree (list ipstr pkey-zkp) vlist))))
)
;; --------------------------------------------------------------------
;; for visual debugging...

#+:LISPWORKS
(progn
  (defmethod children (x layout)
    nil)
  
  (defmethod children ((node node) layout)
    (remove nil (coerce (subs node) 'list)))

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
    (let ((txt (ip node)))
      (make-instance (if (realnode node)
                         'red-text
                       'black-text)
                     :text txt)))

  (defmethod view-tree ((tree node) &key (layout :left-right))
    (unless (emotiq:x11-display-p)
      (pr "No X11 display available to view tree")
      (return-from view-tree nil))
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

(defvar *default-data-file*
  (asdf:system-relative-pathname :emotiq "blockchain/config/cosi-nodes.txt"))
(defvar *default-key-file*
  (asdf:system-relative-pathname :emotiq "blockchain/config/cosi-keying.txt"))

#+nil
(progn
(defun generate-ip ()
  ;; generate a unique random IPv4 address
  (let ((ip (integer-to-dotted-string (random #.(expt 2 32)))))
    (if (gethash ip *ip->node*)
        (generate-ip) ;; should be unlikely, would be 50% at around 2^16 nodes
        (setf (gethash ip *ip->node*) ip))))

(defmethod pair-ip-pkey ((node node))
  ;; used during initial tree generation
  ;; we need numeric values of keys for store in file
  (list (ip node)
        (pkey-zkp node)))

(defmethod pair-ip-pkey ((ip string))
  ;; used during initial tree generation
  ;; we need numeric values of keys for store in file
  (pair-ip-pkey (gethash ip *ip->node*)))

(defun gen-main-tree (leader real-nodes grps)
  (let* ((trees (mapcar #'make-node-tree real-nodes grps))
         (main-tree (find leader trees :test #'string= :key #'ip))
         (all-but (remove main-tree trees))) ;; attach non-leader real nodes to the leader node
    (setf (other-members main-tree)
          (concatenate 'vector (other-members main-tree) all-but))
    (dolist (tree all-but)
      (setf (parent tree) main-tree))
    main-tree))

;; --------------------------------------------------------------
;; Generate Tree / Keying and save to startup init files

(defun generate-tree (&key datafile keyfile (nodes 1000))
  (init-mappings)
  (let* ((leader *leader-node*)
         (real-nodes (remove-duplicates *real-nodes* :test #'string=)))
    
    ;; ensure leader is in the real-nodes collection
    (assert (member leader real-nodes :test #'string=))
    
    ;; pre-populate hash table with real-nodes
    (clrhash *ip->node*)
    (clrhash *pkey->node*)
    (clrhash *pkey->skey*)
    (dolist (ip real-nodes)
      (setf (gethash ip *ip->node*) ip))

    ;; build the trees
    (let* ((nreal (length real-nodes))
           (nodes/grp (ceiling nodes nreal))
           (grps (loop :repeat nreal :collect
                      (loop :repeat nodes/grp :collect
                           (generate-ip))))
           (main-tree (gen-main-tree leader real-nodes grps)))
      
      ;; save nodes as a text file for later
      (ensure-directories-exist *default-key-file* :verbose t)
      (ensure-directories-exist *default-data-file* :verbose t)
      
      (with-open-file (f (or datafile *default-data-file*)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :rename)
        (with-standard-io-syntax
          (let ((*print-readably* t)
                (*print-right-margin* 128))
            (pprint `(:leader     ,leader
                      :real-nodes ,(mapcar #'pair-ip-pkey real-nodes)
                      :groups     ,(mapcar (lambda (grp)
                                             (mapcar #'pair-ip-pkey grp))
                                           grps))
                    f))))
      
      ;; write the pkey/skey associations
      (with-open-file (f (or keyfile *default-key-file*)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :rename)
        (let ((keys (hash-table-alist *pkey->skey*)))
          (with-standard-io-syntax
            (pprint keys f))))
      (assign-bit-positions)
      #+:LISPWORKS (view-tree main-tree)
      (setf *my-node* main-tree
            *top-node* main-tree))))

;; ---------------------------------------------------------------
;; Tree reconstruction from startup init files

(defun read-data-file (path)
  (with-open-file (f path :direction :input)
    (read f)))
      
(defun reconstruct-tree (&key datafile keyfile)
  ;; read the keying file
  (init-mappings)
  (let* ((keys (read-data-file (or keyfile *default-key-file*)))
         (data (read-data-file (or datafile *default-data-file*)))
         (real-nodes (getf data :real-nodes))
         (leader (getf data :leader))
         (grps (getf data :groups)))
    
    ;; sanity checking
    (assert (member leader real-nodes :test #'string= :key #'car))
    (labels ((no-dups (lst)
               (dolist (ip lst)
                 (destructuring-bind (ipstr zkp) ip
                   (assert (null (gethash ipstr *ip->node*)))
                   (let ((pval (keyval (first zkp))))
                     (assert (null (gethash pval *pkey->node*)))
                     (assert (apply #'pbc:check-public-key zkp))
                     (setf (gethash ipstr *ip->node*) ip
                           (gethash pval *pkey->node*) ip))))))
      (clrhash *ip->node*)
      (clrhash *pkey->node*)
      (no-dups real-nodes)
      (mapc #'no-dups grps))
    
    ;; reconstruct keying info
    (clrhash *pkey->skey*)
    (mapc (lambda (pair)
            (destructuring-bind (k . v) pair
              ;; k is integer, compressed pkey ECC pt
              ;; v is skey secret key
              (setf (gethash k *pkey->skey*) v)))
          keys)
    
    ;; reconstruct the trees
    (let ((main-tree (gen-main-tree leader real-nodes grps)))
      (assign-bit-positions)
      #+:LISPWORKS (view-tree main-tree)
      (setf *top-node* main-tree
            *my-node*  main-tree))))

)
