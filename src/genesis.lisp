(in-package :emotiq/config)

(defun get-genesis-block (&key
                            (configuration (settings))
                            (root (emotiq/fs:etc/)))
  (let* ((genesis-block-file (alexandria:assoc-value configuration
                                                     :genesis-block-file))
         (p (make-pathname :name (pathname-name genesis-block-file)
                           :type "loenc"
                           :defaults root)))
    (with-open-file (o p
                     :element-type '(unsigned-byte 8)
                     :direction :input)
      (lisp-object-encoder:deserialize o))))

                               
                                          
          


