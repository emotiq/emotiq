;;; ccl-darwin-special.lisp
;;; Just enough cocoa to get the command-line version of CCL to support ccl::open-url-in-browser
;;; UPDATE: 08-Apr-2018. Don't need this file at all. Just run 'open <url>' as a shell command.

(in-package :ccl)

(unless (find-package :HI)
  (require "OBJC-SUPPORT")
  
  (defparameter *mac-ui-files*
    '("cf-utils"
     ; "libdispatch" ; these aren't necessary
     ; "ccl-application"
     ; "event-process"
     ; "cg"
      ))
  
  (defun load-mac-ui-files (names mac-ui-dir force-compile)
    (let* ((bin-dir (merge-pathnames ";fasls;" mac-ui-dir)))
      (ensure-directories-exist bin-dir)
      (with-compilation-unit ()
        (dolist (name names)
          (let* ((source (make-pathname :name name
                                        :type (pathname-type *.lisp-pathname*)
                                        :defaults mac-ui-dir))
                 (fasl (make-pathname :name name
                                      :type (pathname-type *.fasl-pathname*)
                                      :defaults bin-dir)))
            (when (or force-compile
                      (not (probe-file fasl))
                      (> (file-write-date source)
                         (file-write-date fasl)))
              (compile-file source :output-file fasl :verbose t))
            (load fasl :verbose t))))))
  
  (load-mac-ui-files *mac-ui-files* "ccl:mac-ui;" nil))