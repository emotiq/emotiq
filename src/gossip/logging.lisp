(in-package :gossip)

(defvar *log* nil "Log of gossip actions.")
(defvar *logstream* nil "Log stream for gossip log messages. This is in addition to the *log*, so it's fine if this is nil.")
(defvar *logging-actor* nil "Actor which serves as gatekeeper to *log* to ensure absolute serialization of log messages and no resource contention for *log*.")
(defvar *archived-logs* (make-array 10 :adjustable t :fill-pointer 0) "Previous historical logs")
(defparameter *log-filter* t "t to log all messages; nil to log none")
(defparameter *log-object-extension* ".log" "File extension for object-based logs")
(defparameter *log-string-extension* ".txt" "File extension for string-based logs")
(defparameter *log-dots* nil "True if you want the logger to send a period to *standard-output* for each log message.
    Only used if *logstream* is nil. Mostly for debugging.")

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
  (ac:send *logging-actor* :archive))

(defun log-event (&rest args)
  "General mechanism for logging events. Sends timestamp and args to *logging-actor*.
   No filtering is done here.
   Also returns effective log message."
  (let ((logmsg (cons (usec::get-universal-time-usec) args)))
    (apply 'ac:send *logging-actor* :log logmsg)
    logmsg))

(defun log-event-for-pr (cmd &rest items)
  "Syntactic sugar to play nicely with ac:pr"
  (case cmd
    (:quit (setf ac::*shared-printer-actor* #'ac::blind-print))
    (:init (setf ac::*shared-printer-actor* #'log-event-for-pr))
    (t (apply 'default-logging-function :PR items))))

(defun default-logging-function (logcmd &rest args)
  "Default logger for nodes. Filters using the *log-filter* mechanism."
  (when *log-filter*
    (when (or (eq t *log-filter*)
              (funcall *log-filter* logcmd))
      (apply 'log-event logcmd args))))

;;; <<<<<<< HEAD
;;; ; need to move this to utilities
;;; (defun emotiq/user/root ()
;;;   (let ((dir (asdf:system-relative-pathname :emotiq "../var/log/")))
;;;     (ensure-directories-exist dir)
;;;     dir))
;;; =======
(defun emotiq/log/root ()
  (let ((d (asdf:system-relative-pathname :emotiq "../var/log/")))
    (ensure-directories-exist d)
    d))
;;;>>>>>>> dev

(defun emotiq-log-paths (logvector)
  (let* ((name (format nil "~D-~D" (car (aref logvector 0)) (car (aref logvector (1- (length logvector))))))
         (namelog (concatenate 'string name *log-object-extension*))
         (nametxt (concatenate 'string name *log-string-extension*)))
    (values (merge-pathnames namelog (emotiq/log/root))
            (merge-pathnames nametxt (emotiq/log/root)))))

(defun serialize-log (logvector path)
  "Serialize a log vector to a file as objects. Not thread safe. Don't run
   on log vectors that are in use unless the *logging-actor* does it."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :element-type '(unsigned-byte 8))
    ;(format *standard-output* "Serializing log to ~a" path)
    (lisp-object-encoder:serialize logvector stream)))
    
(defun write-as-string (msg stream)
  "Writes a list of objects (msg) as a string to stream"
  ;; Some of our streams have a lot of overhead on each write, so we pre-convert
  ;;   msg to a string. See Note F.
  (write-string (format nil "~{~S~^ ~}~%" msg) stream))

(defun stringify-log (logvector path)
  "Saves logvector to a file. Moderately thread-safe if copy-first is true.
  Not thread-safe at all otherwise."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output)
    ;(format *standard-output* "Serializing log to ~a" path)
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
  (ac:send *logging-actor* :save))

(defun save-text-log ()
  "Saves current *log* to a file. Thread-safe."
   (ac:send *logging-actor* :save-text))

(defun actor-logger-fn (cmd &rest logmsg)
  "Function that the *logging-actor* runs"
  (case cmd
    (:log (vector-push-extend logmsg *log*)
          ;;; Add message to textual log facility
          (format *error-output* "~&~{~a~^ ~}~&" logmsg))
    ; :save saves current log to files without modifying it
    (:save (%save-log :copy-first nil))
    (:save-text (%save-log :text-only t :copy-first nil))
    ; :archive pushes current log onto *archived-logs*, then starts a fresh log
    (:archive (%archive-log))))
