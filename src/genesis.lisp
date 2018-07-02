(in-package :emotiq/config)

(defun genesis/create (configuration &key
                                       root
                                       (force nil))
  (let ((genesis-block-path (merge-pathnames
                             (alexandria:assoc-value  configuration
                                                      :genesis-block)
                             root)))
    (unless (or force
                (probe-file genesis-block-path))
      (let ((genesis-block
             (cosi/proofs:create-genesis-block
              (alexandria:assoc-value configuration :public)
              (alexandria:assoc-value configuration :witnesses-and-stakes))))
        (with-open-file (o genesis-block-path
                           :direction :output
                           :if-exists :supersede)
          (cl-json:encode-json genesis-block o))))))

