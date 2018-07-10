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

(defun test-mesh (&key (backends '(:tcp :tcp-async)) (endpoints 2) (secs 10) (base-port 3050) (msgsize 1000) (concurrent 10) (window 100))
  (let* ((msg 0)
         (up 0)
         (down 0)
         (log 0)
         (addresses (loop for i from 1 to concurrent collect (format nil "127.~A.~A.~A"
                                                                     (ldb (byte 8 16) i)
                                                                     (ldb (byte 8 8) i)
                                                                     (ldb (byte 8 0) i))))
         (transports nil))
    (setf transports
          (loop for i from 0 below endpoints
             collect (start-transport :backend (elt backends (rem i (length backends)))
                                      :address "0.0.0.0"
                                      :port (+ base-port i)
                                      :peer-up-hook   (lambda (address port)
                                                        (declare (ignore address port))
                                                        (incf up))
                                      :peer-down-hook (lambda (address port why)
                                                        (declare (ignore address port why))
                                                        (incf down))
                                      :message-received-hook (lambda (data)
                                                               (incf msg)
                                                               (transmit (elt transports (random (length transports)))
                                                                         (elt addresses (random (length addresses)))
                                                                         (+ base-port (random endpoints))
                                                                         data))
                                      :log-event-hook (lambda (&rest args)
                                                        (format t "~%Log: ~A~&" args)
                                                        (incf log)))))
    (unwind-protect
         (loop with deadline = (+ (usec:get-universal-time-usec) (* secs 1000000))
            until (>= (usec:get-universal-time-usec) deadline)
            ;; Put initial messages into flight
            initially
              (dolist (tr transports)
                (dotimes (i endpoints)
                  (dotimes (j window)
                    (transmit tr "127.0.0.1" (+ base-port i) (make-array (list msgsize) :initial-element (random 256))))))
            ;; Wait while callbacks bounce the messages around
            do (sleep 0.01))
      (mapc #'stop-transport transports))
    (list :msg msg :up up :down down :log log)))

(defun run-test (&rest parameters)
  (with-open-file (s "test-parameters.txt" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format s "~{~&~A ~A~%~}" parameters)
    (format t "~&PARAMETERS: ~A~%" parameters))
  (handler-case
      (let* ((start (usec:get-universal-time-usec))
             (result (time (apply #'test-mesh parameters)))
             (finish (usec:get-universal-time-usec)))
        (with-open-file (s "test-result.txt" :direction :output :if-exists :supersede :if-does-not-exist :create)
          (let ((x (list* :usec (- finish start) result)))
            (format s "~{~&~A ~A~%~}" x)
            (format t "~&RESULT ~A~%" x))))
    (error (err)
      (with-open-file (s "test-error.txt" :direction :output :if-exists :supersede :if-does-not-exist :create)
        (prin1 err s)))))
  
