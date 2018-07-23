(in-package :emotiq)

(defvar *notestream* *error-output* "Output stream for Emotiq notes")

;;; N.b. deliberately not a macro so we can theoretically inspect the
;;; call stack.

(defun timestring ()
  "Return a string representing current time"
  (let ((formats '(simple-date-time:|yyyymmddThhmmssZ|
                   simple-date-time:|yyyy-mm-dd hh:mm:ss|)))
    (apply (second formats)
           (list (simple-date-time:now)))))

(defun %prefixed-note (prefix message-or-format &rest args)
  (when *notestream*
    (let ((timestamped (concatenate 'string "~&" (timestring) prefix message-or-format)))
      (apply #'format *notestream* timestamped args))))

;;; Do NOT call the following with leading or trailing newlines
;;;  in message-or-format. They will be added automatically.
(defun note (message-or-format &rest args)
  "Emit a note of progress to the appropiate logging system
MESSAGE-OR-FORMAT is either a simple string containing a message, or
a CL:FORMAT control string referencing the values contained in ARGS."
  (apply #'%prefixed-note " " message-or-format args))

(eval-when (:load-toplevel)
  ;; hook, even if Actors isn't loaded...
  ;; If Actors are loaded later, they will respect the hook
  (setf (get :actors :print-handler) (lambda (item)
                                       (note "~A~&" item))))

(defun em-warn (message-or-format &rest args)
  "Like note but this is for warnings."
  (apply #'%prefixed-note " WARN " message-or-format args))
