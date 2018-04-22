;;; gossip-startup.lisp
;;; 21-Apr-2018 SVS
;;; How to start up a node that can participate in gossip

(in-package :gossip)

(defparameter *default-config-path* "/usr/local/gossip/config" "Path to default config file")

(defun read-config-file (config-path)
  (with-open-file (s config-path :direction :input :if-does-not-exist :error)
    (let ((forms (read s nil nil nil)))
      (cond (forms
             (unless (eq :CONFIG (first forms)) ; because we might read :documentation first
               (setf forms (read s nil nil nil)))
             (unless (eq :CONFIG (first forms))
               (error "Can't find :CONFIG form in file ~S" config-path)))
            (t (error "File ~S is empty" config-path)))
      forms)))

(defun gossip-startup (&optional (config-path *default-config-path*))
  (let ((forms (read-config-file config-path)))
    (when forms (destructuring-bind (&key eripa
                                          all-known-addresses
                                          (gossip-port *nominal-gossip-port*)
                                          (numnodes 1)
                                          (preferred-protocol :UDP))
                                    (second forms)
                  (when eripa (setf *eripa* (usocket::host-to-hbo eripa)))
                  (when gossip-port (setf *nominal-gossip-port* gossip-port))
  (clrhash *nodes*) ; kill local nodes
  ; Make required number of local nodes
  ; Clear log, make local node(s) and start server
  (run-gossip-sim preferred-protocol)
  ))))
