(in-package "EMOTIQ")

;; a place-holder for a main Emotiq node
(defun start-node ()
  (gossip:gossip-startup)
  (emotiq/cli:main))

