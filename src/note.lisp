(in-package :emotiq)

#-lispworks(defparameter *notestream* *error-output* "Output stream for Emotiq notes")
#+lispworks(defparameter *notestream* hcl:*background-output* "Output stream for Emotiq notes")

;;; N.b. deliberately not a macro so we can theoretically inspect the
;;; call stack.

(defun logevent (msg &key (level :info) (subsystem :top) data)
  (let ((d (alexandria:plist-alist data)))
    (setf (alexandria:assoc-value d :timestamp) (simple-date-time:now))
    (setf (alexandria:assoc-value d :level) level)
    (setf (alexandria:assoc-value d :msg) msg)
    (setf (alexandria:assoc-value d :subsystem) subsystem)
    d))

(defun logevent-to-string (event)
  (let ((prefix-with-msg (format nil "~&~A ~A ~A msg: ~A" 
                        (simple-date-time:|yyyy-mm-dd hh:mm:ss| (alexandria:assoc-value event :timestamp))
                        (string (alexandria:assoc-value event :level))
                        (string (alexandria:assoc-value event :subsystem))
                        (alexandria:assoc-value event :msg)))
        (stripped-data (alexandria:remove-from-plist (alexandria:alist-plist event) :level :msg :subsystem :timestamp)))
      (if stripped-data
          (format nil "~A | ~{~a: ~a~^, ~}~%" prefix-with-msg stripped-data)
          (format nil "~A~%" prefix-with-msg))))

(defun logevent-to-json (event)
  (cl-json:encode-json-alist-to-string event))

;;; Do NOT call the following with leading or trailing newlines
;;;  in message-or-format. They will be added automatically.
(defun note (message-or-format &rest args)
  "Emit a note of progress to the appropiate logging system
MESSAGE-OR-FORMAT is either a simple string containing a message, or
a CL:FORMAT control string referencing the values contained in ARGS."
  (let* ((msg (apply #'format nil message-or-format args))
        (evt (logevent msg :level :info :subsystem :unknown)))
      (write-string (logevent-to-string evt) *notestream*)
    ))

(defun s-note (msg &key (level :info) (subsystem :unknown) data)
  "Emit a note of progress to the appropiate logging system
MESSAGE-OR-FORMAT is either a simple string containing a message, or
a CL:FORMAT control string referencing the values contained in ARGS."
  (let* ((evt (logevent msg :level level :subsystem subsystem :data data)))
      (write-string (logevent-to-string evt) *notestream*)
    ))

(eval-when (:load-toplevel)
  ;; hook, even if Actors isn't loaded...
  ;; If Actors are loaded later, they will respect the hook
  (setf (get :actors :print-handler) (lambda (item)
                                       (note "~A~%" item))))

(defun em-warn (message-or-format &rest args)
  "Like note but this is for warnings."
  (let* ((msg (apply #'format nil message-or-format args))
        (evt (logevent msg :level :warn :subsystem :unknown)))
    (write-string (logevent-to-string evt) *notestream*))
  )
