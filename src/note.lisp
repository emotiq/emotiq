(in-package :emotiq)

;;; N.b. deliberately not a macro so we can theoretically inspect the
;;; call stack.
(defun note (message-or-format-or-symbol &rest args)
  "Emit a note of progress to the appropiate logging system

MESSAGE-OR-FORMAT-OR-SYMBOL is either a simple string containing a message, or
a CL:FORMAT control string referencing the values contained in ARGS or
a SYMBOL in which case its symbol-name is treated as a simple string (above)."
  (let ((formats '(simple-date-time:|yyyymmddThhmmssZ|
                   simple-date-time:|yyyy-mm-dd hh:mm:ss|)))
    (let ((message-or-format
           (if (symbolp message-or-format-or-symbol)
               (symbol-name message-or-format-or-symbol)
             message-or-format-or-symbol))
          (let ((message 
                 (format nil
                         "~&~a ~a~&"
                         (apply (second formats)
                                (list (simple-date-time:now)))
                         (apply 'format 
                                nil
                                message-or-format
                                (if args args nil)))))
            (format *error-output* message)
            message)))))


