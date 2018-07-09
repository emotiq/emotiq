;;; gossip-startup.lisp
;;; 21-Apr-2018 SVS
;;; How to start up a node that can participate in gossip

(in-package :gossip)

(defparameter *hosts* nil "Cached hosts as read from *hosts-filename* (minus the local machine)")

(defun process-eripa-value (ev)
  (setf *eripa* (if (eq :deduce ev)
                    (eripa)
                    (host-to-vector-quad ev))))

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
      (if (fboundp 'gossip:cosi-loaded-p) ; cosi will fbind this symbol
          (mapc (lambda (pubkey)
                  (make-node ':cosi :pkey pubkey :skey (second (assoc pubkey keypairs))))
                pubkeys)
          (mapc (lambda (pubkey)
                  (make-node ':gossip :uid pubkey))
                pubkeys))
      ;; clear log and start server
      (run-gossip)
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
    (multiple-value-bind (keypairs hosts local-machine stakes)
                         (gossip/config:get-values)
      (configure-local-machine keypairs local-machine)
      ;; make it easy to call ping-other-machines later
      (setq *hosts*
            ;; set up *hosts* with the eripa host removed:
            (loop with eripa-host-hbo = (host-to-hbo (eripa))
                  for host-pair in hosts
                  as host-hbo = (host-to-hbo (first host-pair))
                  when (not (= eripa-host-hbo host-hbo))
                    collect host-pair))
      (setf *stakes* stakes)
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

(defun graceful-shutdown ()
  "Gracefully shutdown the gossip server"
  (shutdown-gossip-server))

(let ((gossip-inited nil))
  (defun gossip-init (&optional (cmd))
    "Call this once before using gossip system. Should return true if everything succeeded."

    ; NOTE: We're not just substituting *logging-actor* here because we want the timestamp to
    ;   be made ASAP after function is called, not when logging finally happens.
    (case cmd
      (:init
       #+OPENMCL
       (if (find-package :gui) ; CCL IDE is running
           (setf emotiq:*notestream* nil) ; just use gossip::*log* in this case
           ; (setf emotiq:*notestream* (hemlock-ext:top-listener-output-stream)) ; another possibility
           (setf emotiq:*notestream* *error-output*))
       ;; In the OpenMCL IDE, outputs to *standard-output* and *error-output* go to a separate listener for the
       ;;   thread the output comes from. Besides causing visual chaos on the screen, it's impossible to
       ;;   see a coherent, serialized log in this environment. Which is why gossip::*log* exists in the first place.
       #-OPENMCL
       (setf emotiq:*notestream* *error-output*)
       (setf *logging-actor* (ac:make-actor 'actor-logger-fn))
       (setf *hmac-keypair-actor* (ac:make-actor 'actor-keypair-fn))
       (archive-log)
       (setf ac::*shared-printer-actor* (ac:make-actor 'log-event-for-pr))     ; make actor:pr calls go to global log.
       (setf gossip-inited t))
      (:maybe
       (unless gossip-inited
         (gossip-init ':init)))
      (:uninit
       (graceful-shutdown)
       (actor-send ac::*shared-printer-actor* ':quit)
       (actor-send *logging-actor* ':quit)
       ;(setf *hmac-keypair-actor* nil)
       (setf gossip-inited nil))
      (:query gossip-inited))))

#|
; might not need this any more
(defun gossip-final-cleanup ()
  "Only call this when lisp quits"
  ; close all open sockets forcibly
  (let* ((connections (connections))
         (sockets (when connections (mapcar 'get-socket connections))))
    (mapc 'usocket:socket-close sockets)))

#+OPENMCL
(eval-when (:load-toplevel :execute)
  (pushnew 'gossip-final-cleanup ccl:*lisp-cleanup-functions*))
|#

; (gossip-startup)
; (mapcar 'unwrap (gossip-startup :ping-others t))
; (mapcar 'unwrap (ping-other-machines (gossip-startup)))

; or
; (gossip-startup)
; (mapcar 'unwrap (ping-other-machines))
