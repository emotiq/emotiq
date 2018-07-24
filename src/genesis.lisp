(in-package :emotiq/config)
(defun get-genesis-block (&key (configuration (settings/read)))
  (let* ((genesis-block-file (alexandria:assoc-value configuration
                                                     :genesis-block-file))
         (p (make-pathname :name (pathname-name genesis-block-file)
                           :type "loenc"
                           ;;; FIXME: a way to have more than one configuration
                           :defaults (emotiq/fs:etc/))))
    (with-open-file (o p
                     :element-type '(unsigned-byte 8)
                     :direction :input)
      (lisp-object-encoder:deserialize o))))

                               
                                          
          


