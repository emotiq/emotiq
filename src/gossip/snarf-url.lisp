;;; snarf-url.lisp
;;; 18-Apr-2018
;;; Blatantly copied from CCL source; modified to be cross-platform

(in-package :gossip)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant wsp #.(let ((str (make-string 6  :element-type 'base-char)))
                      (setf (schar str 0) #\Space)
                      (setf (schar str 1) #\^I)
                      (setf (schar str 2) #\^L)
                      (setf (schar str 3) #\^@)
                      (setf (schar str 4) #\^J)
                      (setf (schar str 5) (code-char #xa0))
                      str)))

(defparameter *maximum-redirects* 10)

#+IGNORE
(defun snarf-url (url &key max-redirects (user-agent "LISP") &aux conn)
  "GET the contents of the url as a (VECTOR (UNSIGNED-BYTE 8))"
  (labels ((is-prefix (prefix string) (eql (length prefix) (string-lessp prefix string)))
	   (header (prefix lines)
	     (let ((line (find prefix lines :test #'is-prefix)))
	       (and line (string-trim wsp (subseq line (length prefix))))))
	   (header-value (prefix lines)
	     (let ((line (find prefix lines :test #'is-prefix)))
	       (and line (parse-integer line :start (length prefix)))))
	   (split-url (string)
             (if (is-prefix "/" string)
                 (list nil 80 string)
                 (if (not (is-prefix "http://" string))
                     (error "Unknown scheme in ~s" string)
                     (let* ((start (length "http://"))
                            (end (length string))
                            (ppos (or (position #\/ string :start start) end))
                            (hend (or (position #\: string :start start :end ppos) ppos)))
                       (list (subseq string start hend)
                             (if (< hend ppos) (parse-integer string :start (1+ hend) :end ppos) 80)
                             (if (< ppos end) (subseq string ppos) "/"))))))
	   (read-header (conn)
	     (loop as lines = (loop for line = (read-line conn nil)
                                until (= 0 (length line)) ; eof or empty line
                                collect line)
               as status = (let ((status-line (pop lines)))
                             (or (parse-integer status-line
                                                :start (position #\Space status-line)
                                                :junk-allowed t)
                                 0))
               while (= status 100)
               finally (return (values lines status)))))
    (unwind-protect
        (loop with original-url = url
          with redirects = (or max-redirects 20)
          with (host port path) = (split-url original-url)
          do (setq conn (ql-network::open-connection host port))
          do (format conn "GET ~a HTTP/1.1~%Host: ~a:~d~%Connection: close~%User-Agent: ~a~2%"
                     path host port user-agent)
          do (finish-output conn)
          do (multiple-value-bind (header-lines status) (read-header conn)
               (when (= status 200)
                 (let ((encoding (header "transfer-encoding:" header-lines)))
                   ;; Here would recognize chunked encoding if cared about that...
                   (when (and encoding (not (string-equal encoding "identity")))
                     (error "Unsupported encoding ~s" encoding)))
                 (return
                  (let* ((count (header-value "content-length:" header-lines)))
                    (if count
                        (let ((vec (make-array count :element-type '(unsigned-byte 8))))
                          (loop for i from 0 below count
                            do (setf (aref vec i) (read-byte conn)))
                          vec)
                        (let ((vec (make-array 1000
                                               :element-type '(unsigned-byte 8)
                                               :fill-pointer 0
                                               :adjustable t)))
                          (loop for byte = (read-byte conn nil) while byte
                            do (vector-push-extend byte vec))
                          (subseq vec 0 (length vec)))))))
               (unless (and (<= 300 status 399) (<= 0 (decf redirects)))
                 (if (<= 300 status 399)
                     (error "Too many redirects")
                     (error "Unknown response ~s" status)))
               (let* ((new (or (header "location:" header-lines)
                               (error "Missing Location: header"))))
                 (destructuring-bind (new-host new-port new-path) (split-url new)
                   (when new-host
                     (setq host new-host port new-port))
                   (setq path new-path))
                 (close conn)
                 (setq conn nil))))
      (when conn (close conn)))))

; rewritten version of http-fetch from quicklisp that takes a stream arg rather than a file
(defun http-fetch (url outstream &key (follow-redirects t)
                       (if-exists :rename-and-delete)
                       (maximum-redirects *maximum-redirects*))
  "default scheme-function for http protocol."
  (let ((redirect-count 0)
        (original-url url)
        (connect-url url)
        (stream (make-broadcast-stream)))
    (loop
      (when (<= maximum-redirects redirect-count)
        (error 'too-many-redirects
               :url original-url
               :redirect-count redirect-count))
      (ql-http::with-connection (connection (ql-http::hostname connect-url) (or (ql-http::port connect-url) 80))
        (let ((cbuf (make-instance 'ql-http::cbuf :connection connection))
              (request (ql-http::request-buffer "GET" url)))
          (ql-http::write-octets request connection)
          (let ((header (ql-http::read-http-header cbuf)))
            (loop while (= (ql-http::status header) 100)
              do (setf header (ql-http::read-http-header cbuf)))
            (cond ((= (ql-http::status header) 200)
                   (let ((size (ql-http::content-length header)))
                     (format stream "~&; Fetching ~A~%" url)
                     (if (and (numberp size)
                              (plusp size))
                         (format stream "; ~$KB~%" (/ size 1024))
                         (format stream "; Unknown size~%"))
                     (save-response file header cbuf
                                    :if-exists if-exists)))
                  ((not (<= 300 (ql-http::status header) 399))
                   (error 'unexpected-http-status
                          :url url
                          :status-code (ql-http::status header))))
            (if (and follow-redirects (<= 300 (ql-http::status header) 399))
                (let ((new-urlstring (ql-http::ascii-header-value "location" header)))
                  (when (not new-urlstring)
                    (error "Redirect code ~D received, but no Location: header"
                           (ql-http::status header)))
                  (incf redirect-count)
                  (setf url (ql-http::merge-urls new-urlstring
                                        url))
                  (format stream "~&; Redirecting to ~A~%" url))
                (return (values header (and file (probe-file file)))))))))))