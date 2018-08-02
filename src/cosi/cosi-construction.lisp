(in-package :emotiq/cosi)

(declaim (optimize (debug 3)))


(defvar *default-data-file*
  (asdf:system-relative-pathname :emotiq "cosi/config/cosi-nodes.txt"))

(defvar *default-key-file*
  (asdf:system-relative-pathname :emotiq "cosi/config/cosi-keying.txt"))


(defvar *comm-ip*  nil) ;; internal use only

(defun make-cosi-tree-node (ipstr pkey-zkp parent)
  (let* ((pkey (first pkey-zkp))
         (pval (key-int-val pkey))
         (skey (gethash pval *pkey->skey*))
         (node (make-instance 'node:node
                              :ip ipstr :real-ip *comm-ip* :parent parent
                              :pkey-zkp pkey-zkp :pkey pkey :skey skey)))
    (setf (gethash ipstr *ip->node*) node
          (gethash pval *pkey->node*) node)))

;; Node construction

(defun partition (node ip-list &key (key #'identity))
  (let* ((bins  (node:make-other-members))
         (nbins (length bins))
         (vnode (dotted-string-to-integer (node:ip node))))
    (mapc (lambda (ip-arg)
            (let* ((vip (dotted-string-to-integer (funcall key ip-arg)))
                   (idx (mod (logxor vnode vip) nbins)))
              (push ip-arg (aref bins idx))))
          ip-list)
    (setf (node:other-members node) bins)))

(defun inner-make-node-tree (ip ip-list &optional parent)
  (multiple-value-bind (ipstr pkeyzkp)
      (gen-node-id ip)
    (let ((node (make-cosi-tree-node ipstr pkeyzkp parent)))
      (when ip-list
        (let ((bins (partition node ip-list
                               :key (lambda (ip-arg)
                                      (if (consp ip-arg)
                                          (car ip-arg)
                                          ip-arg)))))
          (node:iteri-other-members node
                                    (lambda (ix subs)
                                      (setf (aref bins ix)
                                            (inner-make-node-tree (car subs)
                                                                  (cdr subs)
                                                                  node))))
          (node:set-cpu-load node)))
      node)))

(defun make-node-tree (ip vlist)
  "main entry point - captures IPv4 of arg ip for use as real-ip in succeeding nodes"
  (multiple-value-bind (ipstr pkey-zkp)
      (gen-node-id ip)
    (let ((*comm-ip* ipstr))
      (inner-make-node-tree (list ipstr pkey-zkp) vlist))))


(defun dotted-string-to-integer (string)
  (usocket:host-byte-order string))

(defun integer-to-dotted-string (val)
  (usocket:hbo-to-dotted-quad val))


(defun generate-ip ()
  "generate a unique random IPv4 address"
  (let ((ip (integer-to-dotted-string (random #.(expt 2 32)))))
    (if (gethash ip *ip->node*)
        (generate-ip) ;; should be unlikely, would be 50% at around 2^16 nodes
        (setf (gethash ip *ip->node*) ip))))


;; used during initial tree generation
;; we need numeric values of keys for store in file
(defmethod pair-ip-pkey ((node node:node))
  (list (node:ip node)
        (node:pkey-zkp node)))

(defmethod pair-ip-pkey ((ip string))
  (pair-ip-pkey (gethash ip *ip->node*)))


(defun gen-main-tree (leader real-nodes grps)
  (let* ((trees (mapcar #'make-node-tree real-nodes grps))
         (main-tree (find leader trees :test #'string= :key #'node:ip)))
    ;; attach the non-leader real nodes to the leader node
    (let ((all-but (remove main-tree trees)))
      (setf (node:other-members main-tree)
            (concatenate 'vector (node:other-members main-tree) all-but))
      (dolist (tree all-but)
        (setf (node:parent tree) main-tree)))
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
      (ensure-directories-exist *default-key-file*  :verbose t)
      (ensure-directories-exist *default-data-file* :verbose t)
      
      (with-open-file (f (or datafile *default-data-file*)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :rename)
        (with-standard-io-syntax
          (let ((*print-readably* t)
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
        (with-standard-io-syntax
          (pprint (hash-table-alist *pkey->skey*) f)))
      (assign-bit-positions)
      #+:LISPWORKS (view-tree main-tree)
      (setf *my-node* main-tree
            *top-node* main-tree))))

;; ---------------------------------------------------------------
;; Tree reconstruction from startup init files

(defmethod validate-pkey ((pkey pbc:public-key) (psig pbc:signature))
  "only works on primary keys. Can't validate subkeys because signatures require secret key."
  (pbc:check-public-key pkey psig))

(defun read-data-file (path)
  (with-open-file (f path :direction :input)
    (read f)))
      
(defun reconstruct-tree (&key datafile keyfile)
  ;; read the keying file
  (init-mappings)
  (let* ((keys (read-data-file (or keyfile *default-key-file*)))
         (data (read-data-file (or datafile *default-data-file*))))
    (destructuring-bind (&key leader real-nodes groups)
        data
      (assert (member leader real-nodes :test #'string= :key #'car)) ;; sanity checking
      (labels ((no-dups (lst)
                 (dolist (ip lst)
                   (destructuring-bind (ipstr zkp) ip
                     (assert (null (gethash ipstr *ip->node*)))
                     (let ((pval (key-int-val (first zkp))))
                       (assert (null (gethash pval *pkey->node*)))
                       (assert (apply #'validate-pkey zkp))
                       (setf (gethash ipstr *ip->node*) ip
                             (gethash pval  *pkey->node*) ip))))))
        (clrhash *ip->node*)
        (clrhash *pkey->node*)
        (no-dups real-nodes)
        (mapc #'no-dups groups))
    
      ;; reconstruct keying info
      (clrhash *pkey->skey*)
      (mapc (lambda (pair)
              (destructuring-bind (k . v) pair
                ;; k is integer, compressed pkey ECC pt
                ;; v is skey secret key
                (setf (gethash k *pkey->skey*) v)))
            keys)
      
      ;; reconstruct the trees
      (let ((main-tree (gen-main-tree leader real-nodes groups)))
        (assign-bit-positions)
        #+:LISPWORKS (view-tree main-tree)
        (setf *top-node* main-tree
              *my-node*  main-tree)))))

