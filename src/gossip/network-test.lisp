(ql:quickload :gossip)

(in-package :gossip)


;;; ON SERVER MACHINE
(defun setup-server (n)
  "n is starting UID"
  (setf *default-uid-style* :tiny)
  (unless (>= *last-tiny-uid* n)
    (setf *last-tiny-uid* n))
  (clrhash *nodes*)
  (make-graph 10)
  (run-gossip-sim :TCP))

; (setup-server 200)
; (visualize-nodes *nodes*)

;;; ON CLIENT MACHINE

(defparameter rnode nil)
(defparameter localnode nil)

(defun setup-client (n rnodenum)
  "n is starting UID"
  (setf *default-uid-style* :tiny)
  (unless (>= *last-tiny-uid* n)
    (setf *last-tiny-uid* n))
  (clrhash *nodes*)
  (run-gossip-sim :TCP)
  (set-protocol-style :neighborcast)
  (setf rnode (ensure-proxy-node :TCP "localhost" (other-tcp-port) rnodenum))
  (setf localnode (make-node
                     :NEIGHBORS (list (uid rnode)))))

; (setup-client 100 202)
; (visualize-nodes *nodes*)

(defun test-client1 ()
  (archive-log)
  (multiple-value-prog1
      (solicit-direct localnode :count-alive)
    (inspect *log*)))

; (test-client1)

(defun test-client2 ()
  (archive-log)
  (multiple-value-prog1
      (solicit-wait localnode :count-alive)
    (inspect *log*)))

; (test-client2)

(defun test-client3 ()
  (archive-log)
  (multiple-value-prog1
      (solicit-wait localnode :list-alive)
    (inspect *log*)))

; (test-client3)

; only for the actor version of solicit-direct
(defun setup-clientx (n)
  "n is starting UID"
  (setf *default-uid-style* :tiny)
  (unless (>= *last-tiny-uid* n)
    (setf *last-tiny-uid* n))
  (clrhash *nodes*)
  (run-gossip-sim :TCP)
  (set-protocol-style :neighborcast)
  (setf rnode (ensure-proxy-node :TCP "localhost" (other-tcp-port) 0))
  )

; (setup-clientx 100)



; only for the actor version of solicit-direct
(defun test-client1x ()
  (archive-log)
  (multiple-value-prog1
      (solicit-direct rnode :count-alive)
    (inspect *log*)))

; (test-client1x)




;; UPSTREAM replies across network

(defun other-tcp-port ()
  (when *tcp-gossip-socket*
    (if (= *nominal-gossip-port* *actual-tcp-gossip-port*)
        (1+ *nominal-gossip-port*)
        *nominal-gossip-port*)))

(defun setup-client-for-upstream-test (n m other-machine-address)
  "n is starting UID
  m is the UID of a valid node on other-machine-address"
  (setf *default-uid-style* :tiny)
  (unless (>= *last-tiny-uid* n)
    (setf *last-tiny-uid* n))
  (clrhash *nodes*)
  (run-gossip-sim :TCP)
  (set-protocol-style :neighborcast)
  (let ((port (if (equalp "localhost" other-machine-address)
                  (other-tcp-port)
                  *nominal-gossip-port*)))
    (setf rnode (ensure-proxy-node :TCP other-machine-address port m))
    (setf localnode (make-node
                     :NEIGHBORS (list (uid rnode))))))

; (setup-client-for-upstream-test 100 316 "localhost")

(defun test-client-upstream1 ()
  (archive-log)
  (multiple-value-prog1
      (solicit-wait localnode :count-alive)
    (inspect *log*)))

(defun test-client-upstream2 ()
  (archive-log)
  (multiple-value-prog1
      (solicit-wait localnode :list-alive)
    (inspect *log*)))

; (ac::kill-executives)
; (test-client-upstream1)
; (test-client-upstream2)
