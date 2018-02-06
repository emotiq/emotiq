;;; get-mac-hardware-string.lisp
;;; retrieves various key values from command "/usr/sbin/system_profiler SPHardwareDataType"
;;; 05-Feb-2018 SVS

;;; only depends on UIOP
;;; only works on Mac OSX for now.

(defvar *cached-mac-hardware-string* nil)

(defparameter *keys* '("Model Name:"
      "Model Identifier:"
      "Processor Name:"
      "Processor Speed:"
      "Number of Processors:"
      "Total Number of Cores:"
      "L2 Cache (per Core):"
      "L3 Cache:"
      "Memory:"
      "Boot ROM Version:"
      "SMC Version (system):"
      "Serial Number (system):"
      "Hardware UUID:"))

(defun get-mac-hardware-string ()
  (or *cached-mac-hardware-string* ; this should not change over lifetime of this program
      (setf *cached-mac-hardware-string*
            (with-output-to-string (s)
              (uiop/run-program:run-program "/usr/sbin/system_profiler SPHardwareDataType" :output s)))))

(defun get-mac-hardware-value (key)
  (let ((string (get-mac-hardware-string))
        (line nil)
        (pos nil))
    (with-input-from-string (stream string)
      (loop while (not (stream-eofp stream)) do
          (setf line (read-line stream nil nil nil))
        (when (setf pos (search key line))
          (return 
           (string-trim #(#\space) (subseq line (+ (length key) pos)))))))))

(defun get-mac-uuid ()
  (get-mac-hardware-value "Hardware UUID:"))

; (get-mac-uuid)

(dolist (key *keys*)
  (format t "~%~A ~A" key (get-mac-hardware-value key)))