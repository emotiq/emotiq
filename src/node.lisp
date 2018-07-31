(in-package "EMOTIQ")

(defun start-node ()
  "Do whatever is necessary to start a full Emotiq node."
  (gossip:gossip-startup)
  (gossip:ensure-pinger-daemon))


