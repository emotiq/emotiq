(in-package :emotiq)

(defvar *notestream* *error-output* "Output stream for Emotiq notes")

;;; N.b. deliberately not a macro so we can theoretically inspect the
;;; call stack.
(defun note (message-or-format &rest args)
  "Emit a note of progress to the appropiate logging system
MESSAGE-OR-FORMAT is either a simple string containing a message, or
a CL:FORMAT control string referencing the values contained in ARGS."
  (let ((formats '(simple-date-time:|yyyymmddThhmmssZ|
                   simple-date-time:|yyyy-mm-dd hh:mm:ss|)))
    (let ((message 
           (format nil
                   "~&~a ~a~&"
                   (apply (second formats)
                          (list (simple-date-time:now)))
                   (apply 'format 
                          nil
                          message-or-format
                          (if args args nil)))))
      (when *notestream* (format *notestream* message))
      message)))

(eval-when (:load-toplevel)
  ;; hook, even if Actors isn't loaded...
  ;; If Actors are loaded later, they will respect the hook
  (setf (get :actors :print-handler) (lambda (item)
                                       (note "~A" item))))

