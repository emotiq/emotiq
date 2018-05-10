;;; socket-actor-test.lisp
;;; Tests of actor sockets

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

(defun setup-client (&optional server-address)
  ; Start listener socket thread
  (start-gossip-server :TCP)
  (unless server-address (setf server-address (car *server-address*)))
  (let* ((server-port (if (equalp "localhost" server-address)
                          (other-tcp-port)
                          *nominal-gossip-port*))
         (socket-actor (ensure-connection server-address server-port)))
    socket-actor))

(defparameter *sa* (setup-client))

(defun test-sa-client1 ()
  (archive-log)
  (let ((mbox (get-mbox *sa*)))
    (mpcompat:mailbox-read mbox)))

  (multiple-value-prog1
      (solicit-direct localnode :count-alive)
    (inspect *log*)))


  (mpcompat:mailbox-read mbox)