(in-package "EMOTIQ")

(defun start-node ()
  "Do whatever is necessary to start a full Emotiq node."
  (gossip:gossip-startup)
  (let ((random-interval (random 10)))
    (emotiq:note "Sleeping for ~a seconds." random-interval))
  (gossip:ensure-pinger-daemon))


