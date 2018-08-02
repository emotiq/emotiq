(in-package :cosi-simgen)

(declaim (optimize (debug 3)))


(defvar *comm-ip*  nil) ;; internal use only

(defun make-cosi-tree-node (ipstr pkeyzkp parent)
  (let* ((cmpr-pkey (first pkeyzkp))
         (pval      (node::keyval cmpr-pkey))
         (node (make-instance 'node:node
                              :ip      ipstr
                              :skey    (gethash pval node:*pkey->skey*)
                              :pkey    cmpr-pkey
                              :pkey-zkp pkeyzkp
                              :parent  parent
                              :real-ip *comm-ip*
                              )))
    (setf (gethash ipstr node:*ip->node*)   node
          (gethash pval  node:*pkey->node*) node)))

(defun need-to-specify (&rest args)
  (declare (ignore args))
  (error "Need to specify..."))

;; new for Gossip support, and conceptually cleaner...
(defmethod initialize-instance :around ((node node:node) &key &allow-other-keys)
  (setf (node:self node) (make-node-dispatcher node))
  (call-next-method))

(defmethod initialize-instance :after ((node node:node) &key &allow-other-keys)
  (when (null (node:blockchain node))
    (let ((genesis-block (emotiq/config:get-genesis-block)))
      (push genesis-block (node:blockchain node))
      (setf (gethash (block:hash genesis-block)
                     (node:blocks node))
            genesis-block))))

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

;; (defun dotted-string-to-integer (string)
;;   #+:LISPWORKS (comm:string-ip-address string)
;;   #+:OPENMCL   (ccl::dotted-to-ipaddr string)
;;   #+:ALLEGRO   (allegro-dotted-to-integer string)
;;   #-(or :LISPWORKS :ALLEGRO :CLOZURE)
;;   (usocket:host-byte-order string))

;; (defun integer-to-dotted-string (val)
;;   #+:LISPWORKS (comm:ip-address-string val)
;;   #+:OPENMCL (CCL::ipaddr-to-dotted val)
;;   #+:ALLEGRO (allegro-integer-to-dotted val)
;;   #-(or :LISPWORKS :ALLEGRO :CLOZURE)
;;   (usocket:hbo-to-dotted-quad val))

;; (defun keyval (key)
;;   (vec-repr:int key))

;; (defun gen-uuid-int ()
;;   (uuid:uuid-to-integer (uuid:make-v1-uuid)))

;; (defun gen-node-id (ip)
;;   (if (consp ip)
;;       (values-list ip)
;;     (with-accessors ((pkey  pbc:keying-triple-pkey)
;;                      (psig  pbc:keying-triple-sig)
;;                      (skey  pbc:keying-triple-skey))
;;         (pbc:make-key-pair ip)
;;       (setf (gethash (keyval pkey) *pkey-skey-tbl*) skey)
;;       (values ip
;;               (list pkey psig))
;;       )))

;; -------------------------------------------------------------

;; the *NODE-BIT-TBL* is really an ordered vector of all nodes in the
;; tree, ordered by public key magnitude

;; (defvar *node-bit-tbl* #())

;; (defun assign-bits ()
;;   ;; assign bit positions to each node
;;   (let ((collected
;;          (um:accum acc
;;            (maphash (lambda (k node)
;;                       (declare (ignore k))
;;                       (acc node))
;;                     *ip-node-tbl*))))
;;     (setf collected (sort collected '<
;;                           :key (um:compose 'keyval 'node-pkey)))
;;     (loop for node in collected
;;           for ix from 0 do
;;           (setf (node-bit node) ix))
;;     (setf *node-bit-tbl*
;;           (coerce collected 'vector))
;;     ))

;; (defun init-mappings ()
;;   (setf *my-node* nil
;;         *top-node* nil
;;         *leader-node* (get-local-ipv4 "localhost")
;;         *real-nodes*  (mapcar 'cdr *local-nodes*))
;;   (clrhash *ip-node-tbl*)
;;   (clrhash *pkey-node-tbl*)
;;   (clrhash *pkey-skey-tbl*)
;;   (setf *node-bit-tbl* #()))

;; -------------------------------------------------------------------
;; Node construction

(defun partition (node ip-list &key (key 'identity))
  (let* ((bins  (node::make-other-members))
         (nbins (length bins))
         (vnode (node::dotted-string-to-integer (node:ip node))))
    (mapc (lambda (ip-arg)
            (let* ((vip (node::dotted-string-to-integer (funcall key ip-arg)))
                   (ix  (mod (logxor vnode vip) nbins)))
              (push ip-arg (aref bins ix))))
          ip-list)
    (setf (node:other-members node) bins)))

(defun inner-make-node-tree (ip ip-list &optional parent)
  (multiple-value-bind (ipstr pkeyzkp)
      (node::gen-node-id ip)
    (let ((node (make-cosi-tree-node ipstr pkeyzkp parent)))
      (when ip-list
        (let ((bins (partition node ip-list
                               :key (lambda (ip-arg)
                                      (if (consp ip-arg)
                                          (car ip-arg)
                                        ip-arg)))))
          (node::iteri-other-members node
                                     (lambda (ix subs)
                                       (setf (aref bins ix)
                                             (inner-make-node-tree (car subs)
                                                                   (cdr subs)
                                                                   node))))
          (node::set-cpu-load node)))
      node)))

(defun make-node-tree (ip vlist)
  ;; main entry point - captures IPv4 of arg ip for use as real-ip in
  ;; succeeding nodes
  (multiple-value-bind (ipstr pkeyzp)
      (node::gen-node-id ip)
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

;; (defvar *default-data-file* (asdf:system-relative-pathname :cosi-bls "config/cosi-nodes.txt"))
;; (defvar *default-key-file*  (asdf:system-relative-pathname :cosi-bls "config/cosi-keying.txt"))

(defun generate-ip ()
  ;; generate a unique random IPv4 address
  (let ((ip (node::integer-to-dotted-string (random #.(expt 2 32)))))
    (if (gethash ip node:*ip->node*)
        (generate-ip) ;; should be unlikely, would be 50% at around 2^16 nodes
        (setf (gethash ip node:*ip->node*) ip))))

(defmethod pair-ip-pkey ((node node:node))
  ;; used during initial tree generation
  ;; we need numeric values of keys for store in file
  (list (node:ip node)
        (node:pkey-zkp node)))

(defmethod pair-ip-pkey ((ip string))
  ;; used during initial tree generation
  ;; we need numeric values of keys for store in file
  (pair-ip-pkey (gethash ip node:*ip->node*)))

(defun gen-main-tree (leader real-nodes grps)
  (let* ((trees     (mapcar 'make-node-tree real-nodes grps))
         (main-tree (find leader trees
                          :test 'string=
                          :key  'node:ip)))
    ;; attach the non-leader real nodes to the leader node
    (let ((all-but (remove main-tree trees)))
      (setf (node:other-members main-tree)
            (concatenate 'vector
                         (node:other-members main-tree)
                         (apply 'vector all-but)))
      (dolist (tree all-but)
        (setf (node:parent tree) main-tree)))
    main-tree))

;; --------------------------------------------------------------
;; Generate Tree / Keying and save to startup init files

(defun generate-tree (&key datafile keyfile (nodes 1000))
  (node::init-mappings)
  (let* ((leader     node::*leader-node*)
         (real-nodes  (remove-duplicates node::*real-nodes*
                                         :test 'string=)))
    
    ;; ensure leader is in the real-nodes collection
    (assert (member leader real-nodes :test 'string=))
    
    ;; pre-populate hash table with real-nodes
    (clrhash node:*ip->node*)
    (clrhash node:*pkey->node*)
    (clrhash node:*pkey->skey*)
    (dolist (ip real-nodes)
      (setf (gethash ip node::*ip->node*) ip))

    ;; build the trees
    (let* ((nreal       (length real-nodes))
           (nodes/grp   (ceiling nodes nreal))
           (grps        (loop repeat nreal collect
                              (loop repeat nodes/grp collect
                                    (generate-ip))))
           (main-tree   (gen-main-tree leader real-nodes grps)))

      ;; save nodes as a text file for later
      (ensure-directories-exist node::*default-key-file*  :verbose t)
      (ensure-directories-exist node::*default-data-file* :verbose t)
      
      (with-open-file (f (or datafile node::*default-data-file*)
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
      (with-open-file (f (or keyfile node::*default-key-file*)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :rename)
        (let ((keys nil))
          (maphash (lambda (k v)
                     (push (cons k v) keys))
                   node::*pkey->skey*)
          (with-standard-io-syntax
            (pprint keys f))))
      (node::assign-bit-positions)
      #+:LISPWORKS (view-tree main-tree)
      (setf node::*my-node*  main-tree
            node::*top-node* main-tree)
      )))

;; ---------------------------------------------------------------
;; Tree reconstruction from startup init files

(defmethod validate-pkey ((pkey pbc:public-key) (psig pbc:signature))
  ;; only works on primary keys. Can't validate subkeys because
  ;; signatures require secret key.
  (pbc:check-public-key pkey psig))

(defun read-data-file (path)
  (with-open-file (f path
                     :direction :input)
    (read f)))
      
(defun reconstruct-tree (&key datafile keyfile)
  ;; read the keying file
  (node::init-mappings)
  (let* ((key-path  (or keyfile 
                     node::*default-key-file*))
         (keys       (read-data-file key-path))
         (data-path   (or datafile
                          node::*default-data-file*))
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
                   (assert (null (gethash ipstr node:*ip->node*)))
                   (let ((pval (node::keyval (first zkp))))
                     (assert (null (gethash pval node:*pkey->node*)))
                     (assert (apply 'validate-pkey zkp))
                     (setf (gethash ipstr node:*ip->node*) ip
                           (gethash pval  node:*pkey->node*) ip)
                     )))))
      (clrhash node:*ip->node*)
      (clrhash node:*pkey->node*)
      (no-dups real-nodes)
      (mapc #'no-dups grps))
    
    ;; reconstruct keying info
    (clrhash node:*pkey->skey*)
    (mapc (lambda (pair)
            (destructuring-bind (k . v) pair
              ;; k is integer, compressed pkey ECC pt
              ;; v is skey secret key
              (setf (gethash k node:*pkey->skey*) v)))
          keys)
    
    ;; reconstruct the trees
    (let ((main-tree  (gen-main-tree leader real-nodes grps)))
      (node::assign-bit-positions)
      #+:LISPWORKS (view-tree main-tree)
      (setf node::*top-node* main-tree
            node::*my-node*  main-tree)) ;; (gethash (get-my-ipv4) *ip-node-tbl*)
    ))

