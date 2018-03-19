;; cosi-sockets.lisp -- Socket interface for Cosi network
;;
;; We use UDP point-and-shoot protocol among tree nodes.
;;
;; For those relatively few instances where a response is expected,
;; the Actor awaiting a response sets up an ephemeral server on some
;; unused port to use as the return channel.
;;
;; Since all inter-node communications is done with UDP messaging,
;; nodes can reside anywhere in the network, including from multiple
;; processes on the same host.
;;
;; DM/Emotiq  02/18
;; ------------------------------------------------------------
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

#+:ALLEGRO (require :sock)


(in-package :cosi-simgen)

;; ----------------------------

(defparameter *local-ip*    "127.0.0.1")
(defparameter *cosi-port*         65001)

;; ----------------------------
;; AID/Actor Associations for network portable Actor references
;;
;; NOTE: No assumptions on SMP safety of hash table GETHASH,
;;  (SETF GETHASH), and REMHASH

(defclass return-addr ()
  ((ip       :accessor return-addr-ip   ;; the real IPv4 for returns
             :initarg  :ip)
   (port     :accessor return-addr-port ;; the real IPv4 port for returns
             :initarg  :port)
   (aid      :accessor return-addr-aid  ;; actor id for returns
             :initarg  :aid)))

(defparameter *aid-tbl*
  ;; assoc between Actors and AID's
  #+:LISPWORKS (make-hash-table
                :weak-kind :value)
  #+:ALLEGRO   (make-hash-table
                :values :weak)
  #+:CLOZURE   (make-hash-table :weak :value)
  )

(um:defmonitor
    ((associate-aid-with-actor (aid actor)
       (setf (gethash aid *aid-tbl*) actor))
     
     (lookup-actor-for-aid (aid)
       (gethash aid *aid-tbl*))
     
     (unregister-aid (aid)
       (remhash aid *aid-tbl*))
     ))

(defmethod make-return-addr ((ipv4 string) &optional (port *cosi-port*))
  ;; can only be called from within an Actor body. Create a unique AID
  ;; and associate that with the calling Actor for use with received
  ;; messages directed at the Actor. Also construct and return a
  ;; return-addr object, which can be used as a network portable
  ;; reply-to tag.
  (let ((aid  (gen-uuid-int))
        (self (current-actor)))
    (unless self
      (error "MAKE-RETURN-ADDR can only be called by an Actor"))
    (associate-aid-with-actor aid self)
    (make-instance 'return-addr
                   :ip   ipv4
                   :port port
                   :aid  aid)))

(defmethod unregister-return-addr ((ret return-addr))
  (unregister-aid (return-addr-aid ret)))

;; -----------------------------------------------------------

(defmethod send ((node node) &rest msg)
  (unless (node-byz node)
    (socket-send (node-ip node) (node-real-ip node) *cosi-port* msg)))

(defmethod send ((ref return-addr) &rest msg)
  (socket-send ref (return-addr-ip ref) (return-addr-port ref) msg))

(defmethod send ((node null) &rest msg)
  (ac:pr :sent-to-null msg)
  msg)

(defmethod send (dest &rest msg)
  (apply 'ac:send dest msg))

;; -----------------

(defun reply (reply-to &rest msg)
  (apply 'send reply-to :answer msg))

;; --------------------------------------------------------------

(=defun make-hmac (msg skey)
  ;; Every packet sent to another node is accompanied by an HMAC that
  ;; is unforgeable. If a MITM attack occurs, the receiving node will
  ;; fail HMAC verification and just drop the incoming packet on the
  ;; floor. So MITM modifications become tantamount to a DOS attack.
  (pbc:with-crypto (:skey skey)
    (=values (pbc:sign-message msg))))
    

(=defun verify-hmac (packet)
  (let ((decoded (ignore-errors
                   ;; might not be a valid encoding
                   (loenc:decode packet))))
    (when decoded
      (pbc:with-crypto ()
        (when (ignore-errors
                ;; might not be a pbc:signed-message
                (pbc:check-message decoded))
          ;; return the contained message
          (=values (pbc:signed-message-msg decoded)))))
    ))

;; -----------------------------------------------------
;; THE SOCKET INTERFACE...
;; -----------------------------------------------------

(defparameter *max-buffer-length* 65500)
(defparameter *shutting-down*     nil)

(defun port-routing-handler (buf)
  (=bind (packet)
      ;; Every incoming packet is scrutinized for a valid HMAC. If
      ;; it checks out then the packet is dispatched to an
      ;; operation.  Otherwise it is just dropped on the floor.
      (verify-hmac buf)
    
    ;; we can only arrive here if the incoming buffer held a valid
    ;; packet
    (ignore-errors
      ;; might not be a properly destructurable packet
      (destructuring-bind (dest &rest msg) packet
        (let ((true-dest (dest-ip dest)))
          ;; for debug... -------------------
          (when (eq true-dest (node-self *my-node*))
            (pr (format nil "forwarding-to-me: ~A" msg)))
          ;; ------------------
          (apply 'send true-dest msg)))
      )))
    
(defun port-router (buf)
  (let ((handler (load-time-value
                  (make-actor 'port-routing-handler))))
    (send handler buf)))

(defun shutdown-server (&optional (port *cosi-port*))
  (when *my-node*
    (setf *shutting-down* :SHUTDOWN-SERVER)
    (internal-send-socket *local-ip* port "ShutDown")))

(defun socket-send (ip real-ip real-port msg)
  (=bind (packet)
      (make-hmac (list* ip msg) (node-skey *my-node*))
    (internal-send-socket real-ip real-port
                          (loenc:encode packet))))

;; ------------------------------------------------------------------

(defmethod dest-ip ((ip string))
  (let ((node (gethash ip *ip-node-tbl*)))
    (when node
      (node-self node))))

(defmethod dest-ip ((ret return-addr))
  (lookup-actor-for-aid (return-addr-aid ret)))

;; ------------------------------------------------------------------
;; deprecated TCP/IP over Butterfly...

#+:xLISPWORKS
(progn ;; TCP/IP stream over Butterfly
  ;; ------------------
  ;; Client side
  (defun socket-send (ip real-ip msg)
    ;; replace this with USOCKETS protocol
    (let* ((tuple    (make-hmac msg
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
              (when (eql :SHUTDOWN-SERVER *shutting-down*)
                (setf *shutting-down* nil)
                (return-from #1#))
              (let ((saf-buf  (if (eq buf maxbuf)
                                  (subseq buf 0 buf-len)
                                buf)))
                (port-router saf-buf))))
        ;; unwinding
        (usocket:socket-close socket)
        ;; (pr :server-stopped)
        )))
  
  (defun start-ephemeral-server (&optional (port 0))
    (let* ((my-ip  (node-real-ip *my-node*))
	   (socket (usocket:socket-connect nil nil
					   :protocol :datagram
					   :local-host my-ip
					   :local-port port
					   )))
      (mpcompat:process-run-function "UDP Cosi Server" nil
                                     'serve-cosi-port socket)
      (usocket:get-local-port socket)))
       
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
    (declare (ignore ip-address port-num))
    (let ((status (comm:async-io-state-read-status async-io-state)))
      (when status ;; something went wrong
        (pr (format nil "UDP example server: got error ~s, restarting" status))
        (comm:close-async-io-state async-io-state)
        (start-server)
        (return-from #1#))

      (if (eql :SHUTDOWN-SERVER *shutting-down*)
          (progn
            (setf *shutting-down* nil)
            (comm:close-async-io-state async-io-state))
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
          (comm:async-io-state-address async-io-state);; returns address,port
        (declare (ignore ip))
        port)))
      
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
      (when (> nb *max-buffer-length*)
        (error "Packet too large for UDP transmission"))
      (internal-udp-cosi-client-send-request 'comm:close-async-io-state
                                             ip port packet)
      )))

