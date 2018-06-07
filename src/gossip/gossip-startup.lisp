;;; gossip-startup.lisp
;;; 21-Apr-2018 SVS
;;; How to start up a node that can participate in gossip

(in-package :gossip)

(defparameter *keypairs-filename* "keypairs.conf")
(defparameter *hosts-filename* "hosts.conf")
(defparameter *machine-filename* "local-machine.conf")

(defparameter *hosts* nil "Hosts read from *hosts-filename*")

(defparameter *keypair-db-path* nil "Full path to keypairs config file")
(defparameter *hosts-db-path*   nil "Full path to hosts config file")
(defparameter *machine-db-path* nil "Full path to local-machine config file")

(defparameter *config-paths* '("../var/etc/gossip-config/" "config/") "Potential paths relative to :emotiq
    in which to look for config files, more preferred first")

(defun find-root-path ()
  "Look for a viable config directory"
  (some (lambda (subpath)
          (probe-file (asdf:system-relative-pathname :emotiq subpath)))
        *config-paths*))

(defun setup-config-file-paths (&optional (root-path (find-root-path)))
  (when root-path
    (setf *keypair-db-path* (merge-pathnames *keypairs-filename* root-path))
    (setf *hosts-db-path*   (merge-pathnames *hosts-filename* root-path))
    (setf *machine-db-path* (merge-pathnames *machine-filename* root-path))))

(defparameter *whitespace* (list #\space #\tab #\newline #\return #\backspace #\Page))

(defun read-pairs-database (pathname)
  (when (and pathname (probe-file pathname))
    (let ((pair nil)
          (pairs nil))
      (with-open-file (s pathname)
        (setf pairs
              (loop while (setf pair (read s nil nil nil)) collect pair)))
      pairs)))

; (read-pairs-database *keypair-db-path*)

(defun read-local-machine-configuration (&optional (pathname *machine-db-path*))
  (when (probe-file pathname)
    (with-open-file (s pathname :direction :input :if-does-not-exist :error)
      (let ((form (read s nil nil nil)))
        form))))

#+OBSOLETE
(defun read-pubkeys-database (&optional (pathname *pubkeys-db-path*))
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

(defun read-config-files ()
  (let ((keypairs (read-pairs-database *keypair-db-path*))
        (hosts nil)
        (local-machine nil))
    (flet ((badfile (filename)
             (error "~S file not found or invalid" filename)))
      (unless keypairs (badfile *keypairs-filename*))
      (setf hosts (read-pairs-database *hosts-db-path*))
      (unless hosts (badfile *hosts-filename*))
      (setf local-machine (read-local-machine-configuration *machine-db-path*))
      (unless local-machine (badfile *machine-filename*))
      (values keypairs hosts local-machine))))

(defun configure-local-machine (keypairs local-machine)
  "Clear log, make local node(s) and start server"
  (when local-machine
    (destructuring-bind (&key eripa
                              (gossip-port *nominal-gossip-port*)
                              pubkeys)
                        local-machine
      (when eripa (process-eripa-value eripa))
      (cond ((typep gossip-port '(unsigned-byte 16))
             (setf *nominal-gossip-port* gossip-port))
            (t (error "Invalid gossip-port ~S" gossip-port)))
      (cond ((consp pubkeys)
             (clrhash *nodes*)) ; kill local nodes
            (t (error "Invalid or unspecified public keys ~S" pubkeys)))
      ;; check to see that all pubkeys have a match in *keypair-db-path*
      (every (lambda (local-pubkey)
                       (unless (member local-pubkey keypairs :test 'eql :key 'car)
                         (error "Pubkey ~S is not present in ~S" local-pubkey *keypairs-filename*))
                       t)
                     pubkeys)
      ;; make local nodes
      (mapc (lambda (pubkey)
              (make-node :uid pubkey))
            pubkeys)
      ;; clear log and start server
      (run-gossip-sim :TCP)
      t)))

(defun ping-other-machines (&optional (hosts *hosts*))
  "Find what other machines are alive.
  Returns alist of augmented-data or exception monads about live public keys on other machines.
  You can map unwrap on these things to get at the real data."
  (when hosts
    (multiple-list-uids hosts)))

(defun gossip-startup (&key root-path (ping-others nil))
  "Reads initial configuration files.
   If ping-others is true, returns list of augmented-data or exception monads about live public keys on other machines.
   If ping-others is false, just return list of hosts found in *hosts-db-path*.
     You can later call ping-other-machines with this list if desired."
  (let ((*keypair-db-path* *keypair-db-path*)
        (*hosts-db-path* *hosts-db-path*)
        (*machine-db-path* *machine-db-path*))
    (when root-path ; if we're given a root-path, use that instead of the global one
      (setup-config-file-paths root-path))
    (gossip-init ':maybe)
    (multiple-value-bind (keypairs hosts local-machine) (read-config-files)
      (unless (configure-local-machine keypairs local-machine)
        (error "Cannot configure local machine")) ; configure-local-machine should have thrown its own error here
      (setf *hosts* hosts) ; make it easy to call ping-other-machines later
      (if ping-others
          (let ((other-machines (ping-other-machines hosts)))
            (unless other-machines
              (error "No other hosts to check. Perhaps ~S was empty." *hosts-filename*))
            other-machines)
          hosts))))

(let ((gossip-inited nil))
  (defun gossip-init (&optional (cmd))
    "Call this once before using gossip system. Should return true if everything succeeded."
    ; make actor:pr calls go to global log.
    ; NOTE: We're not just substituting *logging-actor* here because we want the timestamp to
    ;   be made ASAP after function is called, not when logging finally happens.
    (case cmd
      (:init
       (let ((root-path (find-root-path)))
         (when root-path
           (ipath:define-illogical-host :emotiq-config root-path) ; in case we need it
           (setup-config-file-paths root-path)))
       #+IGNORE ; #+OPENMCL ; these hemlock streams are just too slow when they get to a couple thousand lines
       ; in CCL, we'll just inspect the *log*
       (if (find-package :gui)
           (setf *logstream* (funcall (intern "MAKE-LOG-WINDOW" :gui) "Emotiq Log"))
           (setf *logstream* *standard-output*))
       #-OPENMCL
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

; (gossip-startup)
; (mapcar 'unwrap (gossip-startup :ping-others t))
; (mapcar 'unwrap (ping-other-machines (gossip-startup)))

; or
; (gossip-startup)
; (mapcar 'unwrap (ping-other-machines))
