(in-package "EMOTIQ")

(defun start-node ()
  "Do whatever is necessary to start a full Emotiq node."
  (gossip:gossip-startup)
  (gossip:ensure-pinger-daemon)
  (gossip:hello (gossip:locate-local-uid-for-graph ':uber)
                (gossip:eripa) 
                (gossip:actual-gossip-port)
                :graphid ':uber))


