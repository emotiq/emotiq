(in-package :emotiq/cosi)

;; --------------------------------------------------------------
;; THE SOCKET INTERFACE, TILL GOSSIP IS UP...
;; --------------------------------------------------------------

(defparameter *cosi-port*   65001)

(defmethod pkey->ip-port ((pkey pbc:public-key))
  (let ((node (gethash (vec-repr:int pkey) *pkey->node*)))
    (values (node:real-ip node) *cosi-port*)))


(defmethod gossip-send (pkey aid msg)
  ;; stub code until we plug in actual Gossip code...
  (if *use-real-gossip-p*
      (gossip:singlecast msg (vec-repr:int pkey) :graphID :uber)
      (multiple-value-bind (ip port)
          (pkey->ip-port pkey)
        (socket-send ip port (or aid pkey) msg))))


(defstruct actor-return-addr
  node aid)


(defun reply (reply-to &rest msg)
  (apply #'send reply-to :answer msg))


(defmethod sdle-store:backend-store-object ((backend sdle-store:resolving-backend)
                                            (obj actors:actor)
                                            stream)
  (let* ((aid (or (ac:get-property obj 'aid) 
                  (setf (ac:get-property obj 'aid) (gen-uuid-int))))
         (pkey (node:pkey (or (node:current-node) *my-node*)))
         (ret (make-actor-return-addr :node pkey :aid aid)))
    (register-actor-id aid obj)
    (sdle-store:backend-store-object backend ret stream)))


(defmethod actors:send ((addr actor-return-addr) &rest msg)
  (with-slots (node aid)
      addr
    (apply #'actors:send node :actor-callback aid msg)))


(defmethod actors:send ((pkey pbc:public-key) &rest msg)
  (if *use-real-gossip-p*
      (if (vec-repr:int= pkey (node:pkey *my-node*))
          (apply #'ac:send (node:self *my-node*) msg)
          (gossip-send pkey nil msg))
      (let ((node (gethash (vec-repr:int pkey) *pkey->node*)))
        (unless node
          (pr (format nil "send to unknown pkey: ~A" (node:short-id pkey))))
        (when node
          (when (node:byzantine-p node)
            (pr (format nil "Byzantine node: ~A" (node:short-id pkey))))
          (unless (node:byzantine-p node)
            (if (eq node node:*current-node*)
                (apply #'ac:send (node:self node) msg)
                (gossip-send pkey nil msg)))))))


(defun make-hmac (msg pkey skey)
  ;; Every packet sent to another node is accompanied by an HMAC that
  ;; is unforgeable. If a MITM attack occurs, the receiving node will
  ;; fail HMAC verification and just drop the incoming packet on the
  ;; floor. So MITM modifications become tantamount to a DOS attack.
  (pbc:sign-message msg pkey skey))


(defun verify-hmac (packet)
  (let ((decoded (ignore-errors ;; might not be a valid encoding
                   (loenc:decode packet))))
    (when (and decoded
               (ignore-errors ;; might not be a pbc:signed-message
                 (pbc:check-message decoded)))
      (pbc:signed-message-msg decoded)))) ;; return the contained message

;; -----------------------------------------------------
;; THE SOCKET INTERFACE...
;; -----------------------------------------------------

(defun port-routing-handler (buf)
  (let ((packet (verify-hmac buf)))
    (unless packet
      (pr "Invalid packet"))
    (when packet
      ;; Every incoming packet is scrutinized for a valid HMAC. If
      ;; it checks out then the packet is dispatched to an
      ;; operation.  Otherwise it is just dropped on the floor.

      ;; we can only arrive here if the incoming buffer held a valid packet
      (progn ;; ignore-errors
        ;; might not be a properly destructurable packet
        (destructuring-bind (dest &rest msg)
            packet
          (let ((node (gethash (vec-repr:int dest) *pkey->node*)))
            (unless node
              (pr :non-existent-node))
            (when node
              (when (eq node *my-node*)
                (pr (format nil "fowarding-by-default-to-me: ~A" msg)))
              (apply #'ac:send (node:self node) msg))))))))


(defvar *port-routing-handler* (make-actor #'port-routing-handler))


;;; For binary delivery, we need to allocate keypair memory at runtime.
(let ((hmac-keypair nil)
      (hmac-keypair-mutex (mpcompat:make-lock)))
  
  (defun hmac-keypair ()
    (unless hmac-keypair
      (mpcompat:with-lock (hmac-keypair-mutex)
        (setf hmac-keypair
              (pbc:make-key-pair (list :port-authority (uuid:make-v1-uuid))))))
    hmac-keypair)
  
  (defmethod socket-send (ip port dest msg)
    (declare (ignore ip port))
    (let* ((payload (make-hmac (list* dest msg)
                               (pbc:keying-triple-pkey (hmac-keypair))
                               (pbc:keying-triple-skey (hmac-keypair))))
           (packet (loenc:encode payload)))
      (ac:send *port-routing-handler* packet))))

