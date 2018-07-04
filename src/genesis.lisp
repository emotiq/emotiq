(in-package :emotiq/config)

(defun genesis/create (configuration &key
                                       (directory (emotiq/fs:tmp/))
                                       (force nil))
  (let ((create-genesis-block (uiop:find-symbol* :create-genesis-block :cosi/proofs)))
    (unless (fboundp create-genesis-block)
      (warn "COSI-BLS ASDF system not loaded; please load manually before attempting genesis.")
      (return-from genesis/create nil))
    (let ((genesis-block-path (merge-pathnames
                               (alexandria:assoc-value configuration
                                                       :genesis-block-file)
                               directory)))
      (when (or force
                (not (probe-file genesis-block-path)))
        (let ((genesis-block
               (funcall create-genesis-block
                        (alexandria:assoc-value configuration :address-for-coins)
                        (alexandria:assoc-value configuration :stakes))))
          (emotiq:note "Writing genesis block as JSON to ~a" genesis-block-path)
          (with-open-file (o genesis-block-path
                             :direction :output
                             :if-exists :supersede)
            (cl-json:encode-json genesis-block o))
        ;;; FIXME: JSON doesn't currently round-trip very well; use LISP-OBJECT-ENCODER as a workaround
          (let ((p (make-pathname :type "loenc"
                                  :defaults genesis-block-path)))
            (with-open-file (o p
                               :element-type '(unsigned-byte 8)
                               :direction :output
                               :if-exists :supersede)
              (lisp-object-encoder:serialize genesis-block o))
            (emotiq:note "Writing genesis block as LOENC to ~a" p))
          genesis-block)))))

(defun get-genesis-block ()
  (with-open-file (o (make-pathname :name (pathname-name
                                           (make-pathname :defaults (alexandria:assoc-value (settings/read)
                                                                                            :genesis-block-file)))
                                    :type "loenc"
                                    :defaults (emotiq/fs:etc/))
                     :element-type '(unsigned-byte 8)
                     :direction :input)
    (lisp-object-encoder:deserialize o)))
                               
                                          
          


