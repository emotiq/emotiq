(in-package :emotiq/config)

(defparameter *stakes-filename*
  (make-pathname :name "stakes" :type "conf"))
(defparameter *max-stake*
  (truncate (/ 1E9 1E3))  ;; ratio of total coins to stakers
  "Maximum amount to award a staked entity")

(defun get-stakes ()
  (let ((p (merge-pathnames *stakes-filename*
                            (emotiq/fs:etc/))))
    (when (probe-file p)
      (with-open-file (o p
                         :direction :input)
        (let ((*read-supress* t))
          (loop :with form
             :while (setf form (read o nil nil nil)) :collecting form))))))
