;;; socket-actor-bench.lisp
;;; Benchmarks of actor sockets, without gossip

(ql:quickload :gossip)
(ql:quickload :ironclad)

(in-package :gossip)

(defun benchmark-sendrecv-tcp-1 (&key (n 1) (seconds 1) (msgsize 100)
                                   (chatty 1.0)
                                   (encode :raw)
                                   (wait :nowait)
                                   (address "localhost"))
  "Run a benchmark of N client-server pairs sending unidirectional messages.
   N is the number of client-server socket pairs to establish.
   SECONDS is the duration of the benchmark.
   CHATTY is the chance of a socket being active on each iteration (0.0 - 1.0).
   ENCODE is :RAW or :SERIALIZE for how the message is encoded.
   MSGSIZE is the message size in octets.
   ADDRESS is the network interface to bind sockets on.
   Returns the rate of client->server messages per second as the benchmark score."
  (let* ((msg (coerce (ironclad:random-data msgsize)
                      (ecase encode
                        (:raw '(simple-array (unsigned-byte 8) *))
                        (:serialize 'string))))
         (listener (usocket:socket-listen address 0
                                          :element-type '(unsigned-byte 8)
                                          :reuse-address t))
         (port (usocket:get-local-port listener))
         (clients '())
         (servers '())
         (messages 0))
    ;; Connnect client-server socket pairs.
    (loop repeat n
       do (push (usocket:socket-connect address port :nodelay :if-supported) clients)
       do (push (usocket:socket-accept listener) servers))
    ;; Helper functions
    (labels ((send ()
               (dolist (client clients)
                 (when (< (random 1.0) chatty)
                   (ecase encode
                     (:raw
                      (write-sequence msg (usocket:socket-stream client)))
                     (:serialize
                      (loenc:serialize msg (usocket:socket-stream client))))
                   (finish-output (usocket:socket-stream client)))))
             (recv ()
               (dolist (server (ecase wait
                                 (:nowait servers)
                                 (:usocket (usocket:wait-for-input servers))))
                 (loop while (listen (usocket:socket-stream server))
                      do (ecase encode
                           (:raw
                            (read-sequence msg (usocket:socket-stream server)))
                           (:serialize
                            (loenc:deserialize (usocket:socket-stream server))))
                      ;; Message received - Score a point.
                      (incf messages))))
             (shutdown ()
               (usocket:socket-close listener)
               (mapc #'usocket:socket-close clients)
               (mapc #'usocket:socket-close servers)))
      (unwind-protect 
           (loop with deadline = (+ (usec:get-time-usec) (* seconds 1000000))
              until (> (usec:get-time-usec) deadline)
              for iterations from 0
              do  (send) (recv)
              finally (return messages))
        (shutdown)))))

(defun suite-sendrecv-tcp-1 (&key
                               (n '(1 10 25 50 100 200))
                               (msgsize '(500 1000 1500 2000))
                               (chatty '(1.0))
                               (seconds '(1.0))
                               (encode '(:raw :serialize))
                               (poll '(:nowait :usocket))
                               output-filename)
  "Run a suite of benchmark-sendrecv-tcp-1 tests.
   Keyword arguments provide the set of values for each variable.
   All permutations are tested and the order of tests is shuffled so
   that background noise (load on machine, heap fragmentation, etc) is
   randomly distributed between all variables.
   
   Output is written to standard output and also OUTPUT-FILENAME if supplied.
   See BENCHMARK-SENDRECV-TCP-1 for more parameter details."
  (let* ((id 0)
         (tests '())
         (output-file (if output-filename
                          (open output-filename :direction :output :if-exists :supersede)
                          nil))
         (output (if output-file
                     (make-broadcast-stream output-file *standard-output*)
                     *standard-output*)))
    ;; Populate TESTS with all permutations of variables.
    (dolist (n n)
      (dolist (msgsize msgsize)
        (dolist (poll poll)
          (dolist (encode encode)
            (dolist (chatty chatty)
              (dolist (seconds seconds)
                (push (list :n (max n 1) :msgsize msgsize :poll poll :encode encode :chatty chatty :seconds seconds) tests)))))))
    ;; Write prelude
    (format t "~&Running ~d tests~%" (length tests))
    (format output "~&id,rate,n,msgsize,duration,poll,encode,chatty~%")
    ;; Execute tests in a randomized order.
    (unwind-protect
         (dolist (test (alexandria:shuffle tests))
           (destructuring-bind (&key n msgsize poll encode chatty seconds) test
             (format output "~&~d,~d,~d,~d,~d,~a,~a,~f~%"
                     (incf id)
                     (benchmark-sendrecv-tcp-1 :n n :msgsize msgsize :seconds seconds)
                     n msgsize seconds poll encode chatty)
             (finish-output output)))
      (unless (null output-file) (close output-file)))
    (format t "~&Benchmark suite completed.~%")))
  
