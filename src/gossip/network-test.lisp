(ql:quickload :gossip)

(in-package :gossip)

(defparameter *aws0* "ec2-35-157-133-208.eu-central-1.compute.amazonaws.com")
(defparameter *aws1* "emq-01.aws.emotiq.ch")
(defparameter *aws2* "emq-02.aws.emotiq.ch")
(defparameter *server-address* *aws0*)
(defparameter *showlog* nil)

;;; ON SERVER MACHINE
(defun setup-server (n)
  "n is starting UID"
  (setf *default-uid-style* :tiny)
  (unless (>= *last-tiny-uid* n)
    (setf *last-tiny-uid* n))
  (clear-local-nodes)
  (make-graph 10)
  (run-gossip-sim :TCP))

; (setup-server 200)
; (visualize-nodes *nodes*)

;;; ON CLIENT MACHINE

(defparameter rnode nil)
(defparameter localnode nil)

(defun setup-client (n server-address rnodenum)
  "n is starting UID"
  (setf *default-uid-style* :tiny)
  (unless (>= *last-tiny-uid* n)
    (setf *last-tiny-uid* n))
  (set-protocol-style :neighborcast)
  (clear-local-nodes)
  (run-gossip-sim :TCP)
  (let ((server-port (if (equalp "localhost" server-address)
                         (other-tcp-port)
                         *nominal-gossip-port*)))
    (setf rnode (ensure-proxy-node ':TCP server-address server-port rnodenum))
    (setf localnode (make-node
                     :neighborhood (list (uid rnode))))))

; (setup-client 100 *server-address* 202)
; (setup-client 100 "localhost" 202)
; (setup-client 100 *server-address* 0) ; for anonymous broadcast
; (visualize-nodes *nodes*)

(defun test-client1 ()
  "Test specific target and direct reply"
  (archive-log)
  (multiple-value-prog1
      (solicit-direct localnode :count-alive)
    (when *showlog*
      (inspect *log*))))

; (test-client1)

(defun test-client2 ()
  "Test specific target and upstream reply"
  (archive-log)
  (multiple-value-prog1
      (solicit-wait localnode :count-alive)
     (when *showlog*
      (inspect *log*))))

; (test-client2)

(defun test-client3 ()
  "Test specific target and upstream reply"
  (archive-log)
  (multiple-value-prog1
      (solicit-wait localnode :list-alive)
     (when *showlog*
      (inspect *log*))))

; (test-client3)

(defun test-client4 ()
  "Test specific target and direct reply"
  (archive-log)
  (multiple-value-prog1
      (solicit-direct localnode :list-alive)
     (when *showlog*
      (inspect *log*))))

; (test-client4)

; only for the actor version of solicit-direct
(defun setup-clientx (n)
  "n is starting UID"
  (setf *default-uid-style* :tiny)
  (unless (>= *last-tiny-uid* n)
    (setf *last-tiny-uid* n))
  (clear-local-nodes)
  (run-gossip-sim :TCP)
  (set-protocol-style :neighborcast)
  (setf rnode (ensure-proxy-node ':TCP "localhost" (other-tcp-port) 0))
  )

; (setup-clientx 100)

; only for the actor version of solicit-direct
(defun test-client1x ()
  (archive-log)
  (multiple-value-prog1
      (solicit-direct rnode :count-alive)
     (when *showlog*
      (inspect *log*))))

; (test-client1x)

;; UPSTREAM replies across network

(defun setup-client-for-upstream-test (n m other-machine-address)
  "n is starting UID
  m is the UID of a valid node on other-machine-address"
  (setf *default-uid-style* :tiny)
  (unless (>= *last-tiny-uid* n)
    (setf *last-tiny-uid* n))
  (clear-local-nodes)
  (run-gossip-sim :TCP)
  (set-protocol-style :neighborcast)
  (let ((port (if (equalp "localhost" other-machine-address)
                  (other-tcp-port)
                  *nominal-gossip-port*)))
    (setf rnode (ensure-proxy-node ':TCP other-machine-address port m))
    (setf localnode (make-node
                     :neighborhood (list (uid rnode))))))

; (setup-client-for-upstream-test 100 316 "localhost")

(defun test-client-upstream1 ()
  (archive-log)
  (multiple-value-prog1
      (solicit-wait localnode :count-alive)
     (when *showlog*
      (inspect *log*))))

(defun test-client-upstream2 ()
  (archive-log)
  (multiple-value-prog1
      (solicit-wait localnode :list-alive)
     (when *showlog*
      (inspect *log*))))

; (ac::kill-executives)
; (test-client-upstream1)
; (test-client-upstream2)
