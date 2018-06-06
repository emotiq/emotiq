;;; gossip-startup.lisp
;;; 21-Apr-2018 SVS
;;; How to start up a node that can participate in gossip

(in-package :gossip)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *config-paths* '("../var/etc/gossip-config/" "config/") "Potential paths relative to :emotiq
    in which to look for config files, more preferred first")
  
  (defun make-config-host ()
    "Look for config directory"
    (some (lambda (subpath)
            (when (probe-file (asdf:system-relative-pathname :emotiq subpath))
              (ipath:define-illogical-host :gossip-config (asdf:system-relative-pathname :emotiq subpath))))
          *config-paths*)))

(eval-when (:load-toplevel :execute) 
  (when (make-config-host)
    (defparameter *keypair-db-file* #P(:gossip-config () "keypairs.conf"))
    (defparameter *hosts-db-file*   #P(:gossip-config () "hosts.conf"))
    (defparameter *pubkeys-db-file* #P(:gossip-config () "pubkeys.conf"))
    (defparameter *gossip-db-file*  #P(:gossip-config () "gossip.conf"))))

(defparameter *whitespace* (list #\space #\tab #\newline #\return #\backspace #\Page))

(defun read-pairs-database (pathname)
  (when (probe-file pathname)
    (let ((pair nil)
          (pairs nil))
      (with-open-file (s pathname)
        (setf pairs
              (loop while (setf pair (read s nil nil nil)) collect pair)))
      pairs)))

; (read-pairs-database *keypair-db-file*)

(defun read-gossip-configuration (&optional (config-path *gossip-db-file*))
  (with-open-file (s config-path :direction :input :if-does-not-exist :error)
    (let ((form (read s nil nil nil)))
      form)))

(defun read-pubkeys-database (&optional (pathname *pubkeys-db-file*))
  "Returns a list of strings, one per public key"
  (when (probe-file pathname)
    (let ((key nil)
          (keys nil))
      (with-open-file (s pathname)
        (setf keys
              (loop while (setf key (read-line s nil nil nil)) collect key)))
      (setf keys (mapcar (lambda (key)
                           (string-trim *whitespace* key))
                         keys))
      (when keys
        (setf keys
              (remove-if (lambda (key)
                           (or (zerop (length key))
                               (eql 0 (position #\; key))))
                         keys))
        (setf keys (mapcar (lambda (string)
                             (parse-integer string))
                           keys))
        keys))))

(defun process-eripa-value (ev)
  (setf *eripa* (if (eq :deduce ev)
                    (eripa)
                    (usocket::host-to-hbo ev))))

(defun gossip-startup (&optional (config-path *gossip-db-file*))
  "Reads initial configuration files. Returns list of lists of host/uids like (address port uid1 uid2 ...)"
  (gossip-init ':maybe)
  (let ((form (read-gossip-configuration config-path))
        (all-pubkeys nil)
        (hosts nil)
        (hosts-uids nil))
    (when form (destructuring-bind (&key eripa
                                         (gossip-port *nominal-gossip-port*)
                                         pubkeys)
                                   form
                 (when eripa (process-eripa-value eripa))
                 (cond ((typep gossip-port '(unsigned-byte 16))
                        (setf *nominal-gossip-port* gossip-port))
                       (t (error "Invalid gossip-port ~S" gossip-port)))
                 (cond ((consp pubkeys)
                        (clrhash *nodes*)) ; kill local nodes
                       (t (error "Invalid or unspecified public keys ~S" pubkeys)))
                 
                 ; Match these against pubkeys in "pubkeys.conf"
                 (setf all-pubkeys (read-pubkeys-database))
                 (cond ((every (lambda (local-pubkey)
                                 (member local-pubkey all-pubkeys :test 'eql))
                               pubkeys)
                        ; if every local pubkey is in the database, make nodes for them
                        (mapc (lambda (pubkey)
                                (make-node :uid pubkey))
                              pubkeys))
                       (t (error "Every local pubkey in ~S does not have a counterpart in ~S"
                                 config-path
                                 *pubkeys-db-file*)))
                 
                 (setf hosts (read-pairs-database *hosts-db-file*))
                 ; Clear log, make local node(s) and start server
                 (cond (hosts
                        (run-gossip-sim :TCP)
                        (let ((uids nil))
                          (setf hosts-uids
                                #+IGNORE
                                (loop for host in hosts
                                  when (setf uids (apply 'list-uids host))
                                  collect (append host uids))
                                (multiple-list-uids hosts)
                                )))
                       (t (error "Hosts file hosts.conf not found or invalid")))
                 hosts-uids))))

(let ((gossip-inited nil))
  (defun gossip-init (&optional (cmd))
    "Call this once before using gossip system. Should return true if everything succeeded."
    ; make actor:pr calls go to global log.
    ; NOTE: We're not just substituting *logging-actor* here because we want the timestamp to
    ;   be made ASAP after function is called, not when logging finally happens.
    (case cmd
      (:init
       #+IGNORE ; #+OPENMCL ; these hemlock streams are just too slow when they get to a couple thousand lines
       ; in CCL, we'll just inspect the *log*
       (if (find-package :gui)
           (setf *logstream* (funcall (intern "MAKE-LOG-WINDOW" :gui) "Emotiq Log"))
           (setf *logstream* *standard-output*))
       #+ (or (not OPENMCL) (not :SHANNON))
       (setf *logstream* *standard-output*)
       (setf *logging-actor* (ac:make-actor #'actor-logger-fn))
       (archive-log)
       (log-event-for-pr ':init)
       (setf gossip-inited t))
      (:maybe
       (unless gossip-inited
         (gossip-init ':init)))
      (:uninit
       (log-event-for-pr ':quit)
       (setf gossip-inited nil))
      (:query gossip-inited))))
     
(eval-when (:load-toplevel :execute)
  (gossip-init :init))

; (gossip-startup)
