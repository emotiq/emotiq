(in-package :emotiq/config)

(defparameter *stakes-filename*
  "stakes.conf")
(defparameter *max-stake* (truncate 1E6))

(defun stakes/generate (pubkeys &key (max-stake *max-stake*))
  "Given a list of PUBKEYS, generate a random stake for each"
  (mapcar (lambda (pubkey)
            (list pubkey (random max-stake)))
          pubkeys))

(defun stakes/write (records &key path)
  (with-open-file (o path
                     :direction :output
                     :if-exists :supersede)
    (format o ";;; ~A~%" *stakes-filename*)
    (format o ";;; THIS FILE IS FOR TESTING ONLY~%")
    (dolist (stake records)
      (format o "~s~%" stake))))

(defun get-stakes ()
  (with-open-file (o (make-pathname :name "stakes" :type "conf"
                                    :defaults (emotiq/fs:etc/))
                     :direction :input)
    (loop :with form
       :while (setf form (read o nil nil nil)) :collecting form)))

  

