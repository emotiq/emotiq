(in-package :cosi-simgen)


(defgeneric node-dispatcher (msg-sym &key &allow-other-keys))

(defun get-local-ipv4 (node-name)
  (assoc-value *local-nodes* node-name :test #'string-equal))


(defvar *my-node* nil
  "which node my machine is on")


(defvar *local-nodes* '(("localhost" . "127.0.0.1"))) ;; Physical network

(defvar *real-nodes* (mapcar #'cdr *local-nodes*))

(defvar *leader-node* (get-local-ipv4 "localhost"))


;; TODO: where are these removed when nodes leaves network?
(defvar *bitpos->node* #()
  "vector of all nodes in the tree ordered by public key magnitude")

(defvar *ip->node* (make-hash-table :test #'equal)
  "XREF from IPv4 address (as string) to Tree Node")

(defvar *pkey->node* (make-hash-table)
  "XREF from PKEY (as compressed ECC pt integer) to Tree Node")

(defvar *pkey->skey* (make-hash-table)
  "PKEY (as compressed ECC pt integer) to SKEY mapping")

(defvar *id->actor* (trivial-garbage:make-weak-hash-table :weakness :value)
  "Association between actor IDs and actor instances (threads)")



(um:defmonitor
    ((register-actor-id (id actor)
       (setf (gethash id *id->actor*) actor))
     (get-actor (id)
       (gethash id *id->actor*))))


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


(defun make-node-dispatcher (node)
  (let ((beh (make-actor
              (lambda (&rest msg)
                (let ((*current-node* node))
                  (apply #'node-dispatcher msg))))))
    (make-actor
     (lambda (&rest msg)
       (emotiq:note "Cosi MSG: ~A" msg)
       (um:dcase msg
         (:actor-callback (actor-id &rest answer)
            (when-let (actor (get-actor actor-id))
              (apply #'send actor answer)))
         (t (&rest msg)
            (apply #'send beh msg)))))))




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


(defun assign-bit-positions ()
  ;; assign bit positions to each node
  (let ((collected (coerce (hash-table-values *ip->node*) 'vector)))
    (setf collected (sort collected #'< :key (compose #'keyval #'node:pkey)))
    (loop
       :for node :across collected
       :for ix :from 0
       :do (setf (node:bitpos node) ix))
    (setf *bitpos->node* collected)))


(defun init-mappings ()
  (setf node:*top-node* nil
        *my-node* nil
        *leader-node* (get-local-ipv4 "localhost")
        *real-nodes*  (mapcar #'cdr *local-nodes*))
  (clrhash *ip->node*)
  (clrhash *pkey->node*)
  (clrhash *pkey->skey*)
  (setf *bitpos->node* #()))
