(in-package "EMOTIQ")

(defun start-node ()
  "Do whatever is necessary to start a full Emotiq node."
  (gossip:gossip-startup)
  (let ((random-interval (random 10)))
    (emotiq:note "Sleeping for ~a seconds." random-interval))
  (unless nil
    #+(or)
    (consp (gossip:ping-other-machines))
    (let* ((host-and-ports
            '(("zt-emq-01.zt.emotiq.ch" 65002)
              ("zt-emq-02.zt.emotiq.ch" 65002)
              ("zt-emq-03.zt.emotiq.ch" 65002)))
           (host-and-port (alexandria:random-elt host-and-ports))
           (host (first host-and-port))
           (port (second host-and-port))
           (public-key (random (expt 2 64)))
           (start-node-id (random (expt 2 64)))) ;; ??? wh
      (emotiq:note "Attempting to say hello from ~a to ~a:~a" public-key host port)
      (gossip::hello public-key host port :graphid :uber))))





