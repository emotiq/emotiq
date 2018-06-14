;;; gossip-startup.lisp
;;; 21-Apr-2018 SVS
;;; How to start up a node that can participate in gossip

(in-package :gossip)

(defparameter *hosts* nil "Hosts as read from gossip/config")

(defun process-eripa-value (ev)
  (setf *eripa* (if (eq :deduce ev)
                    (eripa)
                    (usocket::host-to-hbo ev))))

(defun configure-local-machine (keypairs local-machine)
  "Clear log, make local node(s) and start server"
  (declare (special gossip/config::*keypairs-filename*))
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
             (clear-local-nodes)) ; kill local nodes
            (t (error "Invalid or unspecified public keys ~S" pubkeys)))
      ;; check to see that all pubkeys have a match in *keypair-db-path*
      (every (lambda (local-pubkey)
               (unless (member local-pubkey keypairs :test 'eql :key 'car)
                 (error "Pubkey ~S is not present in ~S" local-pubkey gossip/config::*keypairs-filename*))
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
   Returns a list of augmented-data or exception monads about live UIDs on other machines.
   You can map unwrap on these things to get at the real data.
   Stashes result along with time of acquisition at *live-uids*>"
  (when hosts
    (let ((others (multiple-list-uids hosts)))
      (setf *remote-uids* (cons (get-universal-time) others)
            *uber-set-cache* nil) ; invalidate this cache
      others)))

(defun gossip-startup (&key root-path (ping-others nil))
  "Reads initial testnet configuration optionally attempting a basic connectivity test

ROOT-PATH specifies a specific root the configuration as opposed to
the use of the autoconfiguration mechanism.

If PING-OTHERS is true, returns list of augmented-data or exception
monads about live public keys on other machines.

If PING-OTHERS is false, just return list of hosts found in from the
Gossip testnet configuration.  

The network connectivity test can be invoked via PING-OTHER-MACHINES
with this list if desired."
  ; if we're given a root-path, use that instead of the global one
  (emotiq:note "Starting gossip configuration for testnet…")
  (handler-bind 
      ((error (lambda (e)
                (emotiq:note "Cannot configure local machine: ~a" e))))
    (when root-path 
      (gossip/config:initialize :root-path root-path))
    (gossip-init ':maybe)
    (multiple-value-bind (keypairs hosts local-machine)
        (gossip/config:get-values)
      (configure-local-machine keypairs local-machine)
      ;; make it easy to call ping-other-machines later
      ;; FIXME don't use specials unless you can avoid them…
      (setf *hosts* hosts)
      (emotiq:note "Gossip init finished.")
      (if ping-others
          (handler-bind 
              ((error (lambda (e)
                        (emotiq:note "Failed to run connectivity tests: ~a" e))))
            (let ((other-machines (ping-other-machines hosts)))
              (unless other-machines
                (emotiq:note "No other hosts to check for ping others routine."))
              other-machines))
          hosts))))

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
       #+(or (not :openmcl) (not :shannon))
       (setf *logstream* *error-output*)
       (setf *logging-actor* (ac:make-actor #'actor-logger-fn))
       (setf *hmac-keypair-actor* (ac:make-actor #'actor-keypair-fn))
       (archive-log)
       (log-event-for-pr ':init)
       (setf gossip-inited t))
      (:maybe
       (unless gossip-inited
         (gossip-init ':init)))
      (:uninit
       ;;; XXX Don't we need to shutdown/unbind the socket listener?
       (log-event-for-pr ':quit)
       (setf *logging-actor* nil)
       (setf *hmac-keypair-actor* nil)
       (setf gossip-inited nil))
      (:query gossip-inited))))

; (gossip-startup)
; (mapcar 'unwrap (gossip-startup :ping-others t))
; (mapcar 'unwrap (ping-other-machines (gossip-startup)))

; or
; (gossip-startup)
; (mapcar 'unwrap (ping-other-machines))
