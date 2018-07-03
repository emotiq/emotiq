(in-package :emotiq/config)

(defun genesis/create (configuration &key
                                       root
                                       (force nil))
  (let ((genesis-block-path (merge-pathnames
                             (alexandria:assoc-value configuration
                                                     :genesis-block-file)
                             root)))
    (when (or force
              (not (probe-file genesis-block-path)))
      (let ((genesis-block
             (funcall 
              ;; FIXME: ASDF circular dependency issues! -mhd, 7/3/18
              (intern "CREATE-GENESIS-BLOCK" "COSI/PROOFS") ; hack
              (alexandria:assoc-value configuration :address-for-coins)
              (get-stakes))))
        (with-open-file (o genesis-block-path
                           :direction :output
                           :if-exists :supersede)
          (cl-json:encode-json genesis-block o))
        ;;; FIXME: JSON doesn't currently round-trip very well; use LISP-OBJECT-ENCODER as a workaround
        (with-open-file (o (make-pathname :type "loenc"
                                          :defaults genesis-block-path)
                           :element-type '(unsigned-byte 8)
                           :direction :output
                           :if-exists :supersede)
          (lisp-object-encoder:serialize genesis-block o))))))

(defun get-genesis-block ()
  (with-open-file (o (make-pathname :name (pathname-name
                                           (make-pathname :defaults (alexandria:assoc-value (settings/read)
                                                                                            :genesis-block-file)))
                                    :type "loenc"
                                    :defaults (emotiq/fs:etc/))
                     :element-type '(unsigned-byte 8)
                     :direction :input)
    (lisp-object-encoder:deserialize o)))
                               
                                          
          


