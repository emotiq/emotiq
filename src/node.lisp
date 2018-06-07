(in-package "EMOTIQ")

;;; Startup an Emotiq node
(defun start-node ()
  (gossip:gossip-startup :ping-others t))


