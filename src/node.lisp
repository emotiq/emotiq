(in-package "EMOTIQ")

;;; Startup an Emotiq node

(defun start-node ()
   (gossip:gossip-startup :ping-others nil)
   ;(gossip:start-pinger-daemon) ; haven't written this yet but it will ping others periodically
; must work with David to figure out what to do with the information it returns.
  )