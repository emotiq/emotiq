(in-package :emotiq/cosi)


(defvar *use-gossip-p* t
  "use Gossip graphs instead of Cosi Trees")

(defvar *use-real-gossip-p* t
  "set to T for real Gossip mode, NIL = simulation mode")

(defvar *gossip-neighborhood-graph* nil
  "established neighborhood graph")


(defun ensure-neighborhood-graph (my-node)
  (declare (ignore my-node))
  (or *gossip-neighborhood-graph*
      (setf *gossip-neighborhood-graph*
            (or :UBER ;; for now while debugging
                (gossip:establish-broadcast-group
                 (mapcar #'first (get-witness-list))
                 :graphID :cosi)))))


(defun neighborcast (my-node &rest msg)
  "Gossip-neighborcast - send message to all witness nodes."
  (if *use-real-gossip-p*
      (gossip:broadcast msg
                        :style :neighborcast
                        :graphID (ensure-neighborhood-graph my-node))
      (loop :for node :across *bitpos->node* :do
           (unless (eql node my-node)
             (apply #'send (node:pkey node) msg)))))


(defun broadcast+me (msg)
  (let ((my-pkey (node:pkey (current-node))))
    ;; make sure our own Node gets the message too
    (gossip:singlecast msg my-pkey :graphID nil) ;; force send to ourselves
    ;; this really should go to everyone
    (gossip:broadcast msg
                      :startNodeID my-pkey ; without this we get an error sending to NIL destnode
                      :graphID :UBER)))


(defun sign-message (msg)
  (append msg (list :sig (pbc:sign-hash msg (node:skey (current-node))))))