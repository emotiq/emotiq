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
  (node (node-pkey (current-node)))
  aid)

(defmethod sdle-store:backend-store-object ((backend sdle-store:resolving-backend) (obj ACTORS:ACTOR) stream)
  (let* ((aid  (or (ac:get-property obj 'aid)
                   (setf (ac:get-property obj 'aid) (gen-uuid-int))))
         (ret  (make-actor-return-addr
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
  (let ((node (gethash (int pkey) *pkey-node-tbl*)))
    (unless node
      (pr (format nil "Unknown pkey: ~A" (short-id pkey))))
    (when node
      (when (node-byz node)
        (pr (format nil "Byzantine node: ~A" (short-id pkey))))
      (unless (node-byz node)
        (if (eq node *current-node*)
            (apply 'ac:send (node-self node) msg)
          (gossip-send pkey nil msg))))))

;; --------------------------------------------------------------

(defparameter *local-ip*    "127.0.0.1")
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

(defvar *max-buffer-length* 65500)
(defvar *server-process*    nil)
(defvar *socket*            nil)
(defvar *socket-local-port* nil)

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

(defun port-router (buf)
  (send *handler* (copy-seq buf)))

(defvar *sender* (make-actor (lambda (ip port packet)
                               ;; (pr (format nil "~A ~A ~D ~A" ip port (length packet) packet))
                               (internal-send-socket ip port packet))))

(defun shutdown-server (&optional (port *cosi-port*))
  (when *socket-local-port*
    (setf *socket-local-port* nil)
    (send *sender* *local-ip* port (loenc:encode "ShutDown"))))

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
      ;; (ac:send *sender* ip port packet) ;; for now, while testing...
      (ac:send *handler* packet)
      )))

;; ------------------------------------------------------------------
;; deprecated TCP/IP over Butterfly...

#+:xLISPWORKS
(progn ;; TCP/IP stream over Butterfly
  ;; ------------------
  ;; Client side
  (defun socket-send (ip real-ip msg)
    ;; replace this with USOCKETS protocol
    (let* ((tuple    (make-hmac msg
                                (node-pkey *my-node*)
                                (node-skey *my-node*)))
           (agent-ip (format nil "eval@~A" real-ip)))
      ;; (format t "~%SOCKET-SEND: ~A ~A ~A" ip real-ip msg)
      #+:LISPWORKS
      (bfly:! agent-ip `(forwarding ,ip ',tuple))))

  ;; -------------
  ;; Server side

  (defun forwarding (dest quad)
    ;; (format t "~%FORWARDING: ~A ~A" dest quad)
    (multiple-value-bind (msg t/f) (verify-hmac quad)
      ;; might want to log incomings that fail the HMAC
      ;; just dropping on floor here...
      (when t/f
        (let ((true-dest (dest-ip dest)))
          (when true-dest
            (when (equal true-dest (node-self *my-node*))
              (ac:pr
               (format nil "forwarding-to-me: ~A" msg)))
            (apply 'send true-dest msg))))
      )))

;; ------------------------------------------------------------------
;; USOCKET UDP interface

#-:LISPWORKS
(progn ;; USOCKET interface for ACL

  ;; -------------------
  ;; Server side

  (defun #1=serve-cosi-port (socket)
    (let ((maxbuf (make-array *max-buffer-length*
                              :element-type '(unsigned-byte 8))))
      ;; (pr :server-starting-up)
      (unwind-protect
          (loop
	    (multiple-value-bind (buf buf-len rem-ip rem-port)
		(usocket:socket-receive socket maxbuf (length maxbuf))
	      (declare (ignore rem-ip rem-port))
	      ;; (pr :sock-read buf-len rem-ip rem-port (loenc:decode buf))
              (when (null *socket-local-port*)
                (return-from #1#))
              (let ((saf-buf  (if (eq buf maxbuf)
                                  (subseq buf 0 buf-len)
                                  buf)))
                (port-router saf-buf))))
        ;; unwinding
        (usocket:socket-close socket)
        (setf *server-process*    nil
              *socket*            nil
              *socket-local-port* nil)
        )))

  (defun start-ephemeral-server (&optional (port 0))
    (let* ((my-ip  (node-real-ip *my-node*))
	   (socket (usocket:socket-connect nil nil
					   :protocol :datagram
					   :local-host my-ip
					   :local-port port))
           (server (mpcompat:process-run-function "UDP Cosi Server"
                                                  nil
                                                  #'serve-cosi-port socket)))
      (setf *socket-local-port* (usocket:get-local-port socket)
            *socket* socket
            *server-process* server)))


  (defun start-server ()
    (start-ephemeral-server *cosi-port*))


  ;; -------------
  ;; Client side

  (defun internal-send-socket (ip port packet)
    (let ((nb (length packet)))
      (when (> nb *max-buffer-length*)
        (error "Packet too large for UDP transmission"))
      (let ((socket (usocket:socket-connect ip port
                                            :protocol :datagram)))
        ;; (pr :sock-send (length packet) real-ip packet)
        (unless (eql nb (usocket:socket-send socket packet nb))
          (pr :socket-send-error ip packet))
        (usocket:socket-close socket)
        ))))

;; ------------------------------------------------------------------
;; LW UDP Interface

#+:LISPWORKS
(progn ;; LW UDP Async interface
  (defvar *udp-wait-collection* nil)

  (defun ensure-udp-wait-state-collection ()
    (or *udp-wait-collection*
        (let ((new (comm:create-and-run-wait-state-collection "UDP Cosi Service")))
        (if (sys:compare-and-swap *udp-wait-collection* nil new)
            new
          (progn ;; Another process just set it.
            (comm:wait-state-collection-stop-loop new)
            *udp-wait-collection*)))))

  ;; -------------------------
  ;; Server side

  (defun #1=udp-cosi-server-process-request (async-io-state string bytes-num ip-address port-num)
    (declare (ignore bytes-num ip-address port-num))
    (let ((status (comm:async-io-state-read-status async-io-state)))
      (when status ;; something went wrong
        (pr (format nil "UDP example server: got error ~s, restarting" status))
        (comm:close-async-io-state async-io-state)
        (start-server)
        (return-from #1#))

      (if (null *socket-local-port*)
          (comm:close-async-io-state async-io-state)
        (progn
          (port-router string)
          (udp-cosi-server-receive-next async-io-state)))))

  (defun udp-cosi-server-receive-next (async-io-state )
    (comm:async-io-state-receive-message async-io-state
                                         (make-array *max-buffer-length*
                                                     :element-type '(unsigned-byte 8))
                                         'udp-cosi-server-process-request :needs-address t))

  (defun start-ephemeral-server (&optional (port 0))
    (let ((async-io-state (comm:create-async-io-state-and-udp-socket
                           (ensure-udp-wait-state-collection)
                           :name "UDP Cosi server socket"
                           :local-port port)))
      (udp-cosi-server-receive-next async-io-state)
      (multiple-value-bind (ip port)
          (comm:async-io-state-address async-io-state)  ;; returns address,port
        (declare (ignore ip))
        (setf *socket-local-port* port))))

  (defun start-server ()
    (start-ephemeral-server *cosi-port*))

  ;; -----------------
  ;; Client side

  (defun internal-udp-cosi-client-send-request (callback ip-address ip-port packet)
    (let* ((collection     (ensure-udp-wait-state-collection))
           (async-io-state (comm:create-async-io-state-and-udp-socket collection)))
      (comm:async-io-state-send-message-to-address async-io-state
                                                   ip-address
                                                   ip-port
                                                   packet
                                                   callback)
      async-io-state))

  (defun internal-send-socket (ip port packet)
    (let ((nb (length packet)))
      (ac:pr (format nil "internal-send-socket nb=~A" nb))
      (when (> nb *max-buffer-length*)
        (error "Packet too large for UDP transmission"))
      (internal-udp-cosi-client-send-request 'comm:close-async-io-state
                                             ip port packet)
      )))
