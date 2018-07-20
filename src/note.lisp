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

(defun %note (message-or-format &rest args)
  "Format a log message and return it"
  (let ((message 
         (format nil
                 "~a~%"
                 (apply 'format 
                        nil
                        message-or-format
                        args))))
    message))

; This can be redefined to be actor-based for proper serialization
(defun record-note (&rest message-strings)
  (when *notestream*
    (dolist (msg message-strings)
      (write-string msg *notestream*))))

;;; Do NOT call the following with leading or trailing newlines
;;;  in message-or-format. They will be added automatically.

(defun note (message-or-format &rest args)
  "Emit a note of progress to the appropiate logging system
MESSAGE-OR-FORMAT is either a simple string containing a message, or
a CL:FORMAT control string referencing the values contained in ARGS."
  (let ((timestring (timestring))
        (outstring (apply '%note message-or-format args)))
    (record-note timestring " " outstring)
    outstring))

(defun em-warn (message-or-format &rest args)
  "Like note but this is for warnings."
  (let ((timestring (timestring))
        (outstring (apply '%note
                          (concatenate 'string "WARN: " message-or-format)
                          args)))
    (record-note timestring " " outstring)
    outstring))
  