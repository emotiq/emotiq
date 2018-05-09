(ql:quickload :gossip)

(in-package :gossip)

(defparameter rnode nil)
(defparameter localnode nil)

(defun setup-client (n)
  "n is starting UID"
  (setf *default-uid-style* :tiny)
  (unless (>= *last-tiny-uid* n)
    (setf *last-tiny-uid* n))
  (clrhash *nodes*)
  (run-gossip-sim :TCP)
  (set-protocol-style :neighborcast)
  (setf rnode (ensure-proxy-node :TCP "localhost" (other-tcp-port) 0))
  (setf localnode (make-node
                     :NEIGHBORS (list (uid rnode)))))

; (setup-client 100)
; (visualize-nodes *nodes*)

(defun setup-server (n)
  "n is starting UID"
  (setf *default-uid-style* :tiny)
  (unless (>= *last-tiny-uid* n)
    (setf *last-tiny-uid* n))
  (clrhash *nodes*)
  (make-graph 1)
  (run-gossip-sim :TCP))

; (setup-server 200)
; (visualize-nodes *nodes*)

(defun test-client1 ()
  (archive-log)
  (multiple-value-prog1
      (solicit-direct localnode :count-alive)
    (inspect *log*)))

(defun test-client2 ()
  (archive-log)
  (multiple-value-prog1
      (solicit-wait localnode :count-alive)
    (inspect *log*)))

; (test-client1)
; (test-client2)