(in-package "EMOTIQ")

(defun start-node ()
  "Do whatever is necessary to start a full Emotiq node."
  (gossip:gossip-startup)
  (let ((random-interval (random 10)))
    (emotiq:note "Sleeping for ~a seconds." random-interval))
  (gossip:ping-other-machines)
  
  ;;; Shannon notes:
  ;;; haven't written this yet but it will ping others periodically
  ;;; must work with David to figure out what to do with the information it
  #+(or)
  (gossip:start-pinger-daemon)

  (emotiq/tracker:query-current-state))


