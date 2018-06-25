(defpackage test-harness
  (:use :cl)
  (:export :test-system))

(in-package :test-harness)

(defun test-system (system)
  (mp:initialize-multiprocessing)
  (handler-case
      (asdf:make system)
    (error (e)
      (format *standard-output*
              "~&Failed to Quickload dependencies for system `~a`:~&~a~&" system e)
      (uiop:quit -1)))
  (handler-case
      (if (asdf:test-system system)
          (progn
            (format *standard-output* "~&Test-system succeeded for system: `~a`~&" system)
            (uiop:quit 0))
          (progn
            (format *standard-output* "~&Test-system failed for system: `~a`~&" system)
            (uiop:quit -1)))
    (error (e)
      (format *standard-output*
              "~&Testing system `~a` signalled error:~&~a~&" system e)
      (uiop:quit -1))))
