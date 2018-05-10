;;; socket-actor-test.lisp
;;; Tests of actor sockets

(ql:quickload :gossip)

(in-package :gossip)

(defparameter *server-address* "localhost")

(defun other-tcp-port ()
  "Deduce proper port for other end of connection based on whether this process
   has already established one. Only used for testing two processes communicating on one machine."
  (when *tcp-gossip-socket*
    (if (= *nominal-gossip-port* *actual-tcp-gossip-port*)
        (1+ *nominal-gossip-port*)
        *nominal-gossip-port*)))

(defun setup-server ()
  ; Start listener socket thread
  (start-gossip-server :TCP))

(defparameter *stop-loopback* nil)

(defun loopback-server ()
  "Wait for an object to appear -- from anywhere -- and echo it."
  (setup-server)
  (setf *stop-loopback* nil)
  (loop until *stop-loopback* do
    (multiple-value-bind (object valid) (mpcompat:mailbox-read *incoming-mailbox* 10)
      (when valid (print object)
        (destructuring-bind (reply-actor msg) object
          (ac:send reply-actor :send-socket-data (concatenate 'string msg "GOO")))))))

; (trace ac:send)
; (loopback-server)

(defun setup-client ()
  ; Start listener socket thread
  (start-gossip-server :TCP))

(defun test-sa-client1 (&optional server-address)
  "Assumes (loopback-server) is running on server"
  (archive-log)
  (setup-client)
  (unless server-address (setf server-address *server-address*))
  (let* ((server-port (if (equalp "localhost" server-address)
                          (other-tcp-port)
                          *nominal-gossip-port*))
         (socket-actor (ensure-connection server-address server-port))
         (mbox (get-outbox socket-actor)))
    (ac:send socket-actor :send-socket-data "Foo")
    (mpcompat:mailbox-read mbox 10)))

; (test-sa-client1) ; should return (<some actor> "FooGOO")
