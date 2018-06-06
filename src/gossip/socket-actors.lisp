;;; socket-actors.lisp
;;; Actor-protected TCP sockets

(in-package :gossip)

(defvar *tcp-connection-table* (kvs:make-store ':hashtable :test 'eql) "Memoizes all open connections")
(defvar *poll-thread* nil "Process that polls for network input.")
(defvar *socket-actor-table* (make-hash-table :test 'eq) "Hash table from socket to socket-actor.")

;; ------------------------------------------------------------------------------
;; Generic handling for expected authenticated messages. Check for
;; valid deserialization, check deserialization is a
;; pbc:signed-message, check for valid signature, then call user's
;; handler with embedded authenticated message. If any failure along
;; the way, just drop the message on the floor.

(defun do-process-authenticated-packet (deserialize-fn body-fn)
  "Handle decoding and authentication. If fails in either case just do nothing."
  (let ((decoded (ignore-errors
                   ;; might not be a valid serialization
                   (funcall deserialize-fn))))
    (when (and decoded
               (ignore-errors
                 ;; might not be a pbc:signed-message
                 (pbc:check-message decoded)))
      (funcall body-fn (pbc:signed-message-msg decoded)))))

(defmacro with-authenticated-packet ((packet-arg) deserializer &body body)
  "Macro to simplify decoding and authentication"
  `(do-process-authenticated-packet (lambda ()
                                      ,deserializer)
                                    (lambda (,packet-arg)
                                      ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-authenticated-packet" 2)

; Patch for usocket to ensure it doesn't see any local IPv6 addresses.
;  It's a problem specific to Lispworks but this patch doesn't hurt anything in CCL.
(defun usocket::get-host-by-name (name)
  (let ((hosts (usocket::get-hosts-by-name name)))
    (setf hosts (remove-if-not (lambda (vector) (= 4 (length vector))) hosts))
    (car hosts)))

; herein we sometimes refer to these things as "connections"
(defclass socket-actor (ac:actor)
  ()
  (:documentation "An actor whose job is to gatekeep one end of a two-way TCP connected socket"))

; Probably should convert this to usocket:wait-for-input at some point, but I haven't tested it yet.
(defun wait-for-stream-input (stream &optional timeout)
  "Returns stream when stream is ready for input. Nil if timed-out or -- if timeout itself is nil -- if stream has been closed.
  Second value will be true if stream has definitely been closed or POLLHUPed.
  [Second value may be a numeric error code on some implementations.]"
  #+OPENMCL
  (multiple-value-bind (success errno status)
                       (ccl::process-input-wait* (ccl::socket-device stream) (when timeout (* 1000 timeout)))
    (cond ((and (null success) ; if success and errno are both nil, we timed out without error
                (null errno))
           ; When no further data available, fall through and end loop
           (if timeout
               (values nil nil)     ; stream might have data later--it merely timed out
               (values nil :DEAD))) ; stream will never have more data, because fn returned but nil timeout
          ((null success) ; stream will never have more data
           (if (not (zerop (logand status CCL::*POLLHUP*)))
               (values nil :POLLHUP) ; other end hung up
               (values nil errno)))
          (t (values stream nil))))
  #+LISPWORKS
  (let ((streams (system:wait-for-input-streams (list stream) :timeout timeout)))
    (cond (streams (values (car streams) nil))
          (t (if timeout
                 (values nil 
                         (if (open-stream-p stream)
                             nil
                             :STREAM-CLOSED)) ; stream might have data later--it just timed out
                 (values nil :DEAD))))) ; stream will never have more data, because fn returned but nil timeout
  )


(defun stream-eofp (stream)
  "Returns true if stream is at EOF"
  #+OPENMCL
  (ccl:stream-eofp stream)
  #+LISPWORKS
  (stream:stream-check-eof-no-hang stream))

(defun make-ip-key (address port)
  (declare (ignore port))
  (usocket::host-to-hbo address) ; ignore port for now. Cannot in general expect incoming port to match *nominal-gossip-port*.
  #+IGNORE
  (if (and (integerp address)
           (null port))
      address ; means port is already folded into real-address
      (+ (ash (usocket::host-to-hbo address) 16) port)))

(defun memoize-connection (address port object)
  (let ((key (make-ip-key address port)))
    (kvs:relate-unique! *tcp-connection-table* key object)))
  
(defun unmemoize-connection (address port)
  (let ((key (make-ip-key address port)))
    (kvs:remove-key! *tcp-connection-table* key)))

(defun lookup-connection (address port)
  "Returns open pipe to address/port, if any"
  (let* ((key (make-ip-key address port))
         (actor (kvs:lookup-key *tcp-connection-table* key))
         (process (when actor (ac:get-property actor :thread))))
    (when actor
      (cond ((and process ; check for valid process/thread
                  #+OPENMCL (not (ccl::process-exhausted-p process)))
             actor)
            (t (ac:set-property actor :thread (make-select-thread actor))
               actor))
      actor)))

;;; Socket-actor property functions. These can be called safely by anybody--not just by the actor itself.
(defmethod get-socket ((sa socket-actor))
  "Returns socket this actor controls"
  (ac:get-property sa :socket))

(defmethod get-outbox ((sa socket-actor))
  "Returns outbox this actor controls"
  (ac:get-property sa :outbox))

(defmethod get-thread ((sa socket-actor))
  "Returns select-loop thread that monitors this actor's socket"
  (ac:get-property sa :thread))

(defmethod get-peer-address ((sa socket-actor))
  "Returns peer-address of this socket"
  (ac:get-property sa :peer-address))

(defmethod get-peer-port ((sa socket-actor))
  "Returns peer-port of this socket"
  (ac:get-property sa :peer-port))

;;; End of actor property functions

(defun send-socket-receive-message (actor)
  "Send a socket-receive message to an actor."
  (ac:send actor :receive-socket-data))

(defun send-socket-shutdown-message (actor &optional reason)
  "Send a socket-shutdown message to an actor."
  
  (ac:send actor :shutdown reason))

(defun select-loop ()
  "Loop that monitors read-readiness of socket and sends messages
  to actor when ready. This should be run in a separate
  thread, not in an actor.
  (If you run it in an actor, that actor and the thread containing
  it will block, and that actor will no longer be able to receive
  messages.)"
  (when (debug-level 3)
    (log-event "Poll thread started") (mpcompat:current-process))
  (unwind-protect
      (loop
       ;; Detect the sockets with input ready to read.
       ;; Wake up at least once per 10ms so that new sockets don't wait
       ;; too long to be included in the poll set.
       (dolist (socket (sockets-with-input-waiting :timeout 0.01))
         (let ((actor (gethash socket *socket-actor-table*)))
           (when (debug-level 4)
             (log-event "Poll activity" actor socket))
           (multiple-value-bind (success errorp)
               (wait-for-stream-input (usocket:socket-stream socket) nil)
             (cond (success (send-socket-receive-message actor))
                   (t ;; If success is null here, stream is irreparably dead with
                      ;; no further data available, so fall through and end loop
                    (send-socket-shutdown-message actor errorp)
                    (remhash socket *socket-actor-table*)
                    (return)))))))
    ;; This thread is not polling anymore.
    (when (debug-level 3)
      (log-event "~&Poll thread terminated~%" (mpcompat:current-process)))
    (setf *poll-thread* nil)))

(defun sockets-with-input-waiting (&key timeout)
  "Return the list of actor sockets that have input waiting."
  (usocket:wait-for-input (alexandria:hash-table-keys *socket-actor-table*)
                          :ready-only t
                          :timeout timeout))

(defmethod make-select-thread ((actor socket-actor))
  "Launch a thread to monitor a socket for incoming data. If such occurs, send
   a message to the socket's actor and return to monitoring."
  (mpcompat:process-run-function "Select loop" nil 'select-loop))

(defmethod socket-actor-dispatcher (&rest msg)
  "Dispatch function for a socket-actor"
  (let ((socket-cmd (first msg))
        (actor (ac:current-actor)))
    (labels ((handle-stream-error (errcode stream)
               (cond ((open-stream-p stream)
                      (if (stream-eofp stream)
                          (ac:self-call :shutdown :EOF)
                          (ac:self-call :shutdown errcode)))
                     (t (ac:self-call :shutdown :CLOSED)))
               nil)
             (safe-listen (stream)
               (handler-case (listen stream)
                 (error (c) (handle-stream-error c stream)))))
      (when actor
        (let ((socket (get-socket actor)))
          (case socket-cmd
            (:send-socket-data
             (let* ((socket (get-socket actor))
                    (stream (usocket:socket-stream socket))
                    (payload (second msg)))
               (cond ((and (usocket:stream-usocket-p socket)
                           (open-stream-p stream))
                      (loenc:serialize payload stream)
                      (finish-output stream))
                     (t (ac:self-call :shutdown :CLOSED)))))
            (:receive-socket-data
             (let ((outbox (get-outbox actor))
                   (stream (usocket:socket-stream socket))
                   (object nil))
               (when (safe-listen stream) ; it is ****VERY**** important to check listen here.
                 ; Why? Because many :receive-socket-data messages can pile up because the concurrent select-loop can show the socket has data
                 ;      _while_ loenc:deserialize is happening. The result will be that (current-actor) will hang in the next loenc:deserialize
                 ;      because there won't really be any data waiting, because the previous loenc:deserialize used it all up.
                 ;      We should probably set a flag in the actor and use a state machine to better coordinate between
                 ;      the select-loop and the code here, and prevent unnecessary :receive-socket-data messages, but for now we'll just check listen.
                 (when (debug-level 4)
                   (log-event "Socket receive" actor))
                 (setf object
                       (handler-case (loenc:deserialize stream)
                         (error (c) (handle-stream-error c stream))))
                 (ac:send outbox actor object) ; first parameter is this actor, so we can know where object came from
                 )))
            (:shutdown
             ; Kill the select thread, close the socket. Leave outbox alone in case any output objects remain.
             (let ((socket (get-socket actor))
                   (address (get-peer-address actor))
                   (port (get-peer-port actor))
                   (reason (second msg)))
               (when (debug-level 3)
                 (log-event "Socket shutdown" actor reason))
               (when (usocket:stream-usocket-p socket) (usocket:socket-close socket))
               (unmemoize-connection address port)))))))))

(defun open-active-socket (address port)
  "Open an active TCP connection from this machine to another.
   If second value is non-nil, it indicates some kind of error happened that makes first value invalid."
  (when (debug-level 3)
    (log-event "Opening socket to " (usocket::host-to-vector-quad address) port))
  (multiple-value-bind (socket errorp)
                       (handler-case (usocket:socket-connect address port :protocol ':stream :element-type '(unsigned-byte 8))
                         (T (e) (values nil e)))
    (cond ((and (not errorp)
                (usocket:stream-usocket-p socket))
           socket)
          (t (values socket (or errorp :INVALID-SOCKET))))))

(defun ofsag (source-actor object) ; output function socket-actor gossip
  "The output function for socket-actors used in gossip. This will only be called (via ac:send) from
   a socket-actor monitoring a connection for incoming data."
  (let ((rem-address (get-peer-address source-actor)))
    (with-authenticated-packet (packet)
      object ; has already been deserialized at this point
      (destructuring-bind (destuid srcuid rem-port msg) packet
        (when (debug-level 1)
          (log-event :INCOMING-TCP msg :FROM rem-address :TO destuid))
        ;ensure a local node of type proxy-gossip-node exists on this machine with
        ;  given rem-address, rem-port, and srcuid (the last of which will be the proxy node's real-uid that it points to).
        (let ((proxy (ensure-proxy-node :TCP rem-address rem-port srcuid)))
          (incoming-message-handler msg (uid proxy) destuid) ; use uid of proxy here because destuid needs to see a source that's meaningful
          ;   on THIS machine.
          )))))

(defun ensure-connection (address port &optional (outbox 'ofsag))
  "Find or make an actor-mediated connection to given address and port.
   This will often locate an extant incoming connection that was left open: That's precisely the idea.
   If second value is non-nil, it indicates some kind of error happened that makes first value invalid."
  (or (lookup-connection address port)
      (multiple-value-bind (socket errorp)
                           (open-active-socket address port)
        (cond ((not errorp)
               (make-socket-actor socket outbox))
              (t (values socket errorp))))))
      
(defun make-socket-actor (socket &optional outbox)
  "Wraps an actor around an open socket connection. Returns the actor.
   You can pass in your own argument for outbox; if nil a mailbox will be created automatically.
   Given outbox could be either a mailbox, an actor, or a function that acts as a continuation
   for handling received objects."
  (ensure-poll-thread)
  (let* ((outbox (or outbox (mpcompat:make-mailbox)))
         (actor  (make-instance 'socket-actor :fn 'socket-actor-dispatcher))
         (address (usocket:get-peer-address socket))
         (port    (usocket:get-peer-port socket)))
    (ac:set-property actor :peer-address address)
    (ac:set-property actor :peer-port    port)
    (ac:set-property actor :outbox outbox)
    (ac:set-property actor :socket socket)
    (memoize-connection address port actor)
    (setf (gethash socket *socket-actor-table*) actor)
    actor))

(defun ensure-poll-thread ()
  "Ensure that the socket-actors socket input poll thread has been started."
  ;; XXX detect when it has died and needs a restart.
  (when (null *poll-thread*)
    (setf *poll-thread*
          (mpcompat:process-run-function "socket actors poll" nil 'select-loop))))

