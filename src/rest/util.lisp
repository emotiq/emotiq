(in-package :emotiq-rest)

(defun mime-type (path)
  "Return mime type corresponding to suffix of PATH"
  (flet ((ends-with (suffix string)
           (alexandria:ends-with-subseq suffix string :test 'string-equal)))
    (cond
      ((ends-with "gif" path)
       "image/gif")
      ((ends-with "ttl" path)
       "text/turtle")
      ((ends-with "png" path )
       "image/png")
      ((or
        (ends-with "jpg" path)
        (ends-with "jpeg" path))
       "image/jpeg")
      ((ends-with  "css" path)
       "text/css")
      ((or (ends-with "js" path)
           (ends-with "json" path))
       "application/javascript")
      ((ends-with  "eot" path)
       "application/vnd.ms-fontobject")
      ((ends-with  "n3" path)
       "text/n3")
      ((or  ;; HACK: but better to represent as text
        (ends-with "md" path)
        (ends-with "markdown" path)
        (ends-with  "org" path))
       "text/plain")
      ((ends-with  "nq" path)
       "application/n-quads")
      ((ends-with "ttf" path)
       "application/octet-stream")
      ((or
        (ends-with "htm" path)
        (ends-with "html" path))
       "text/html")
      ((or
        (ends-with "woff" path)
        (ends-with "woff2" path))
       "application/x-font-woff")
      ((ends-with "svg" path)
       "image/svg+xml")
      (t
       (note "Failed to determine mime type for '~A'." path)
       "image/*"))))

(defmacro as-html (&body body)
  `(who:with-html-output-to-string (o)
     ,@body))

(defun reify (relative-uri)
  "Return the full path of RELATIVE-URI to this system.

Returns the mime type of the file as the second value."
  ;;; TODO We only handle <file:> scheme with funk merge to ASDF
  ;;; definitions.  Generalize this.
  (let ((result (asdf:system-relative-pathname :emotiq-rest relative-uri)))
    (if (probe-file result)
        (values
         result
         (mime-type (pathname-type result)))
        "No bindable representation found.")))

(defun note (message-or-format &rest args)
  "Emit a note of progress to the appropiate logging system."
  (format t 
          "~a ~a~%"
;;          (simple-date-time:|yyyymmddThhmmssZ|
          (simple-date-time:|yyyy-mm-dd hh:mm:ss|
                            (simple-date-time:now))
          (apply 'format 
                 nil
                 message-or-format
                 (if args 
                     args
                     nil))))
