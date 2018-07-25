(in-package :gossip)

(defvar *log* nil "Log of gossip actions.")
(defvar *logging-actor* 'ac:do-nothing "Actor which serves as gatekeeper to *log* to ensure absolute serialization of log messages and no resource contention for *log*.")
(defvar *archived-logs* (make-array 10 :adjustable t :fill-pointer 0) "Previous historical logs")
(defparameter *log-filter* t "t to log all messages; nil to log none")
(defparameter *log-object-extension* ".log" "File extension for object-based logs")
(defparameter *log-string-extension* ".txt" "File extension for string-based logs")
(defvar *debug-level* 1 "True to log debugging information while handling gossip requests. Larger values log more messages.")

(defun debug-level (&optional (level nil level-supplied-p))
  (cond (level-supplied-p
         (when *debug-level*
           (if (numberp *debug-level*)
               (if (numberp level)
                   (>= *debug-level* level)
                   nil)
               t)))
        (t *debug-level*)))

(defun (setf debug-level) (val)
  (setf *debug-level* val))

; Logcmd: Keyword that describes what a node has done with a given message UID
; Examples: :IGNORE, :ACCEPT, :FORWARD, etc.
(defmethod node-log ((node abstract-gossip-node) logcmd msg &rest args)
  "Log a message-based event that occurred to a node."
  (when (logfn node)
    (apply (logfn node)
           logcmd
           node
           msg
           args)))

(defun edebug (level tag &rest args)
  "Syntactic sugar for wrapping debug-level around a log-event call"
  (when (debug-level level)
     (typecase tag 
       (abstract-gossip-node (apply 'node-log tag args))
       (t (apply 'log-event tag args)))))

(defun log-exclude (&rest strings)
  "Prevent log messages whose logcmd contains any of the given strings. Case-insensitive."
 (lambda (logcmd)
   (let ((logcmdname (symbol-name logcmd)))
     (notany (lambda (x) (search x logcmdname :test #'char-equal)) strings))))

(defun log-include (&rest strings)
  "Allow log messages whose logcmd contains any of the given strings. Case-insensitive."
 (lambda (logcmd)
   (let ((logcmdname (symbol-name logcmd)))
     (some (lambda (x) (search x logcmdname :test #'char-equal)) strings))))

;; Ex: Don't log any messages that contain "WAIT" or "ACCEPT"
;; (setf *log-filter* (log-exclude "WAIT" "ACCEPT" "IGNORE"))

;; Ex: Don't log any messages that contain "COALESCE"
;; (setf *log-filter* (log-exclude "COALESCE"))

;; Ex: Include only :FINALREPLY messages
;; (setf *log-filter* (log-include "FINALREPLY"))

;; Ex: Don't include SIR messages
;; (setf *log-filter* (log-exclude "SIR"))


(defun new-log ()
  "Returns a new log space"
  (make-array 10 :adjustable t :fill-pointer 0))

;;; TODO: This doesn't call save-log. It probably should once saving is thoroughly debugged.
(defun %archive-log ()
  "Archive existing *log* and start a new one.
   This function is not thread-safe; should only be called from *logging-actor*."
  (vector-push-extend *log* *archived-logs*)
  (setf *log* (new-log)))

(defun archive-log ()
  "Archive existing *log* and start a new one. Thread-safe."
  (actor-send *logging-actor* :archive))

(defun log-event (&rest args)
  "General mechanism for logging events. Sends timestamp and args to *logging-actor*.
  No filtering is done here.
  Also returns effective log message."
  (let ((logmsg (cons (usec::get-universal-time-usec) args)))
    ; Don't use actor-send here. If we get an error while trying to log, just drop it on the floor.
    ;   Using actor-send would cause an infinite loop.
    (handler-case (progn (apply 'ac:send *logging-actor* :log logmsg)
                    logmsg)
      (error () nil))))

(defun log-event-for-pr (cmd &rest items)
  "Syntactic sugar to play nicely with ac:pr. Note this is NOT an actor function, so don't call actor-only functions from it."
  (case cmd
    (:quit (ac:become 'ac::blind-print))
    (t (apply 'default-logging-function items))))

(defun default-logging-function (logcmd &rest args)
  "Default logger for nodes. Filters using the *log-filter* mechanism."
  (when *log-filter*
    (when (or (eq t *log-filter*)
              (funcall *log-filter* logcmd))
      (apply 'log-event logcmd args))))

(defun emotiq-log-paths (logvector)
  (let* ((name (format nil "~D-~D" (car (aref logvector 0)) (car (aref logvector (1- (length logvector))))))
         (namelog (concatenate 'string name *log-object-extension*))
         (nametxt (concatenate 'string name *log-string-extension*)))
    (values (merge-pathnames namelog (emotiq/fs:var/log/))
            (merge-pathnames nametxt (emotiq/fs:var/log/)))))

(defun serialize-log (logvector path)
  "Serialize a log vector to a file as objects. Not thread safe. Don't run
   on log vectors that are in use unless the *logging-actor* does it."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :element-type '(unsigned-byte 8))
    ;(format *standard-output* "Serializing log to ~a" path)
    ;; must not use FORMAT to *standard-output* here, because this is being
    ;; run in a multiprocessing environment (Actors or no Actors)
    (lisp-object-encoder:serialize logvector stream)))
    
(defun write-as-string (msg stream)
  "Writes a list of objects (msg) as a string to stream"
  ;; Some of our streams have a lot of overhead on each write, so we pre-convert
  ;;   msg to a string. See Note F.
  (write-string (format nil "~{~S~^ ~}~%" msg) stream))
  ;; FORMAT NIL here is maybe OK, this is being run in a multiprocesing environment, but,
  ;; even if FORMAT is swapped out, WRITE-STRING cannot run until it has all of its args
  ;; in place.  It depends on whether WRITE-STRING is "atomic" in a given implementation.
  ;; WRITE-STRING takes two arguments - a string and a stream.  Maybe full-preemption
  ;; will carve up the WRITE-STRING exucution, maybe not.  So, this function depends on
  ;; the implementation - if WRITE-STRING is not defined as atomic, then it is possible
  ;; that the msg will be interleaved (the effects of multiprocessing (forget about
  ;; Actors) might be to time-slice WRITE-STRING, only less likely).  (Actors or no Actors).
  ;; The bugs simply become more elusive, as args get smaller.  Bugs happen less frequently
  ;; and then appear to be "random".  The "window" for interleaving is much smaller, but can
  ;; can happen once in a while (which makes errors so much harder to debug).

(defun stringify-log (logvector path)
  "Saves logvector to a file. Moderately thread-safe if copy-first is true.
  Not thread-safe at all otherwise."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output)
    ;(format *standard-output* "Serializing log to ~a" path)
    ;; must not use FORMAT to *standard-output* here, because this is being
    ;; run in a multiprocessing environment (Actors or no Actors)
    (loop for msg across logvector do
      (write-as-string msg stream))))

(defun deserialize-log (path)
  "Deserialize object-based log file at path"
  (with-open-file
      (o path :direction :input :element-type '(unsigned-byte 8))
    (lisp-object-encoder:deserialize o)))

(defun %save-log (&key (text-only nil) (copy-first t))
  "Saves current *log* to two files -- one as objects and one as just strings.
   (The first facilitates forensic debugging; the second is just for human administrator consumption.)
   Moderately thread-safe if copy-first is true.
   Not thread-safe at all otherwise."
  (let ((newlog (if copy-first
                    (copy-seq *log*)
                    *log*)))
    (declare (dynamic-extent newlog))
    (multiple-value-bind (path-for-objects path-for-strings)  (emotiq-log-paths newlog)
      (unless text-only
        (serialize-log newlog path-for-objects))
      (stringify-log newlog path-for-strings)
      (values path-for-objects
              path-for-strings))))

(defun save-log ()
  "Saves current *log* to a file. Thread-safe."
  (actor-send *logging-actor* :save))

(defun save-text-log ()
  "Saves current *log* to a file. Thread-safe."
   (actor-send *logging-actor* :save-text))

(defun actor-logger-fn (cmd &rest logmsg)
  "Function that the *logging-actor* runs"
  (case cmd
    (:log (vector-push-extend logmsg *log*)
          ;;; Shunt message to line-oriented log facility
          ;; First object in logmsg is the timestamp.
          ;;
          ;; TODO: use the timestamp directly. It's (car logmsg).
          (emotiq:note "~{~a~^ ~}" (cdr logmsg)))
    (:quit (ac:become 'ac:do-nothing))
          
    ; :save saves current log to files without modifying it
    (:save (%save-log :copy-first nil))
    (:save-text (%save-log :text-only t :copy-first nil))
    ; :archive pushes current log onto *archived-logs*, then starts a fresh log
    (:archive (%archive-log))))

