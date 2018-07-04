(in-package :emotiq/config)

(defparameter *stakes-filename*
  (make-pathname :name "stakes" :type "conf"))
(defparameter *max-stake*
  (truncate (/ 1E9 1E3))  ;; ratio of total coins to stakers
  "Maximum amount to award a staked entity")

(defun stakes/generate (public-keys &key (max-stake *max-stake*))
  "Given a list of PUBLIC-KEYS, generate a random stake for each

Returns the alist of public key to stake cons cells.

"
  (mapcar (lambda (pkey)
            (cons pkey (random max-stake)))
          public-keys))

(defun stakes/write (records &key path)
  (with-open-file (o path
                     :direction :output
                     :if-exists :supersede)
    (format o ";;; ~A~%" *stakes-filename*)
    (format o ";;; THIS FILE IS FOR TESTING ONLY~%")
    (dolist (stake records)
      (format o "~s~%" stake))))

(defun get-stakes ()
  (let ((p (merge-pathnames *stakes-filename*
                            (emotiq/fs:etc/))))
    (when (probe-file p)
      (with-open-file (o p
                         :direction :input)
        (let ((*read-supress* t))
          (loop :with form
             :while (setf form (read o nil nil nil)) :collecting form))))))




  

