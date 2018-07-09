(ql:quickload :emotiq)
(ql:quickload :alexandria)


(let* ((all-tests.sexp
        (asdf:system-relative-pathname :emotiq "../etc/all-tests.sexp"))
       (s (alexandria:read-file-into-string all-tests.sexp))
       (systems (read-from-string s)))
  (format *standard-output* "~{~s ~}" systems))

