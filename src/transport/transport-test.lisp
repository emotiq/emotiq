(in-package :transport/test)

(defun test-open-close (&key (backend :tcp) port)
  (let ((transport (start-transport :backend backend :address "localhost" :port port
                                    :message-received-hook (lambda (&rest msg) (format t "~&Msg: ~A~%" msg)))))
    (sleep 0.1)
    (unwind-protect
         (dotimes (i 10)
           (transmit transport
                     "localhost"
                     port
                     (vector 0 1 2 3 4 (mod i 256))))
      (sleep 1)
      (stop-transport transport))))

(defun test-mesh (&key (backends '(:tcp :tcp-async)) (endpoints 10) (loops 10000) (base-port 3050))
  (let* ((n 0)
         (msg (lambda (&rest msg)
                (declare (ignore msg))
                (incf n)))
         (transports (loop for i from 1 to endpoints
                        collect (start-transport :backend (elt backends (rem i (length backends)))
                                                 :address "localhost"
                                                 :port (+ base-port i)
                                                 :message-received-hook msg
                                                 :log-event-hook (lambda (&rest args) (sleep 0.1) (format t "~&~A~%" args) (finish-output))))))
    (unwind-protect
         (progn
           (dotimes (i loops)
             (dolist (tr transports)
               (dotimes (i endpoints)
                 (transmit tr "localhost" (+ base-port i) (vector 1 2 3 i)))))
           (sleep 1))
      (mapc #'stop-transport transports))
    n))

