;; cosi-netw-xlat.lisp -- Interrim layer for comms until Gossip is fully absorbed
;;
;; DM/Emotiq  04/18
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

;; -----------------------------------------------------------------

(defparameter *aid-tbl*
  ;; assoc between Actors and AID's
  (trivial-garbage:make-weak-hash-table :weakness :value))

(um:defmonitor
    ((associate-aid-with-actor (aid actor)
       (setf (gethash aid *aid-tbl*) actor))

     (lookup-actor-for-aid (aid)
       (gethash aid *aid-tbl*))
     ))

;; -----------------------------------------------------------------

(defstruct actor-return-addr
  node aid)

(defmethod sdle-store:backend-store-object ((backend sdle-store:resolving-backend) (obj ACTORS:ACTOR) stream)
  (let* ((aid  (or (ac:get-property obj 'aid)
                   (setf (ac:get-property obj 'aid) (gen-uuid-int))))
         (ret  (make-actor-return-addr
                :node (node-pkey (or (current-node)
                                     *my-node*))
                :aid  aid)))
    (associate-aid-with-actor aid obj)
    (sdle-store:backend-store-object backend ret stream)))

(defmethod ac:send ((addr actor-return-addr) &rest msg)
  (apply 'ac:send (actor-return-addr-node addr)
         :actor-callback (actor-return-addr-aid addr) msg))

;; -----------------

#|
(defmethod ac:send ((node node) &rest msg)
  (when (node-byz node)
    (pr (format nil "Byzantine-node: ~A" (short-id node))))
  (unless (node-byz node)
    (if (eq node *current-node*)
        (apply 'ac:send (node-self node) msg)
      (apply 'ac:send (node-pkey node) msg))))
|#

;; -----------------

(defun reply (reply-to &rest msg)
  (apply 'send reply-to :answer msg))

;; -----------------------------------------------------------

(defmethod gossip-send (pkey aid msg)
  ;; stub code to do direct UDP until we plug in actual Gossip code...
  (cond (*use-real-gossip*
         (gossip:singlecast msg (int pkey)
                            :graphID :uber))

        (t
         (multiple-value-bind (ip port)
             (translate-pkey-to-ip-port pkey)
           (let ((dest (or aid pkey)))
             (socket-send ip port dest msg))))
        ))

(defmethod ac:send ((pkey pbc:public-key) &rest msg)
  (cond (*use-real-gossip*
         (if (int= pkey (node-pkey *my-node*))
             (apply 'ac:send (node-self *my-node*) msg)
           (gossip-send pkey nil msg)))
        
        (t
         (let ((node (gethash (int pkey) *pkey-node-tbl*)))
           (unless node
             (pr (format nil "send to unknown pkey: ~A" (short-id pkey))))
           (when node
             (when (node-byz node)
               (pr (format nil "Byzantine node: ~A" (short-id pkey))))
             (unless (node-byz node)
               (if (eq node *current-node*)
                   (apply 'ac:send (node-self node) msg)
                 (gossip-send pkey nil msg))))))
        ))

;; --------------------------------------------------------------

(defparameter *cosi-port*   65001)

(defmethod translate-pkey-to-ip-port ((pkey pbc:public-key))
  (let ((node (gethash (int pkey) *pkey-node-tbl*)))
    (values (node-real-ip node) *cosi-port*)))

;; --------------------------------------------------------------
;; THE SOCKET INTERFACE, TILL GOSSIP IS UP...
;; --------------------------------------------------------------

(defun make-hmac (msg pkey skey)
  ;; Every packet sent to another node is accompanied by an HMAC that
  ;; is unforgeable. If a MITM attack occurs, the receiving node will
  ;; fail HMAC verification and just drop the incoming packet on the
  ;; floor. So MITM modifications become tantamount to a DOS attack.
  (pbc:sign-message msg pkey skey))


(defun verify-hmac (packet)
  (let ((decoded (ignore-errors
                   ;; might not be a valid encoding
                   (loenc:decode packet))))
    (when (and decoded
               (ignore-errors
                 ;; might not be a pbc:signed-message
                 (pbc:check-message decoded)))
      ;; return the contained message
      (pbc:signed-message-msg decoded))
    ))

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

      ;; we can only arrive here if the incoming buffer held a valid
      ;; packet
      (progn ;; ignore-errors
        ;; might not be a properly destructurable packet
        (destructuring-bind (dest &rest msg) packet
          (let ((node (gethash (int dest) *pkey-node-tbl*)))
            (unless node
              (pr :Non-existent-node))
            (when node
              (when (eq node *my-node*)
                (pr (format nil "fowarding-by-default-to-me: ~A" msg)))
              (apply 'ac:send (node-self node) msg)))))
      )))

(defvar *handler* (make-actor 'port-routing-handler))

;;; For binary delivery, we need to allocate keypair memory at
;;; runtime.
(let ((hmac-keypair nil)
      (hmac-keypair-mutex (mpcompat:make-lock)))
  (defun hmac-keypair ()
    (unless hmac-keypair
      (mpcompat:with-lock (hmac-keypair-mutex)
        (setf hmac-keypair
              (pbc:make-key-pair (list :port-authority (uuid:make-v1-uuid))))))
    hmac-keypair)
  (defmethod socket-send (ip port dest msg)
    (let* ((payload (make-hmac (list* dest msg)
                               (pbc:keying-triple-pkey (hmac-keypair))
                               (pbc:keying-triple-skey (hmac-keypair))))
           (packet  (loenc:encode payload)))
      (ac:send *handler* packet)
      )))

