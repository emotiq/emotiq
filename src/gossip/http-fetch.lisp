;;; http-fetch.lisp
;;; 18-Apr-2018
;;; Only depends on Quicklisp.
;;; Lots of code herein copied from Quicklisp.
;;; Only handles http, not https. Which means it won't work for urls that automatically
;;;   redirect http to https, which is most of them nowadays.

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

#|
  (with-open-file (stream file
                          :direction :output
                          :if-exists :rename-and-delete
                          :element-type 'octet)
|#

(defun make-stream-writer (stream)
  "Create a callback for writing data to STREAM."
  (if (subtypep (stream-element-type stream) 'character)
      (lambda (data start end)
        (loop for i from start below end do
          (write-char (code-char (aref data i)) stream)))
      (lambda (data start end)
        (write-sequence data stream :start start :end end))))

(defun save-chunk-response (stream cbuf)
  "For a chunked response, read all chunks and write them to STREAM."
  (let ((fun (make-stream-writer stream))
        (matcher (ql-http::acode-matcher :cr :lf)))
    (loop
     (let ((chunk-size (ql-http::read-chunk-header cbuf)))
       (when (zerop chunk-size)
         (return))
       (ql-http::call-for-n-octets chunk-size fun cbuf)
       (ql-http::skip-until-matching matcher cbuf)))))

(defun save-response-to-stream (stream header cbuf)
  (let ((content-length (ql-http::content-length header)))
    (cond ((ql-http::chunkedp header)
           (save-chunk-response stream cbuf))
          (content-length
           (ql-http::call-for-n-octets content-length
                              (make-stream-writer stream)
                              cbuf))
          (t
           (ql-http::call-until-end (make-stream-writer stream) cbuf)))))

; rewritten version of http-fetch from quicklisp that takes a stream arg rather than a file
(defun http-fetch (url outstream &key (follow-redirects t)
                       (maximum-redirects *maximum-redirects*))
  "default scheme-function for http protocol."
  (setf url (ql-http::url url))
  (let ((redirect-count 0)
        (original-url url)
        (connect-url url)
        (stream (make-broadcast-stream)))
    (loop
      (when (<= maximum-redirects redirect-count)
        (error 'ql-http::too-many-redirects
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
                     (save-response-to-stream outstream header cbuf)))
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
                (return (values header outstream)))))))))

; Test
; (with-output-to-string (s) (http-fetch "http://quicklisp.org/" s))
; (with-output-to-string (s) (http-fetch "http://api.ipify.org/" s))

#+TEST
(let ((s (make-string-output-stream)))
  (multiple-value-bind (header stream)
                       (http-fetch "http://www.cnn.com/" s :follow-redirects nil) ; redirects to https
    (values header
            (get-output-stream-string stream))))