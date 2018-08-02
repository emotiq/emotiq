(in-package :emotiq/config)

(defun get-genesis-block (&key
                            (root (emotiq/fs:etc/)))
  ;;; This is wrong:  use a special rather than the pathname
  (let* ((genesis-block-file emotiq/config:*genesis-block-filename*)
         (p (merge-pathnames (make-pathname :name (pathname-name genesis-block-file)
                                            :type "loenc")
                             root)))
    (with-open-file (o p
                     :element-type '(unsigned-byte 8)
                     :direction :input)
      (lisp-object-encoder:deserialize o))))

                               
                                          
          


