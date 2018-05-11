(in-package :ccl)

;; Poll and EOF:
;; http://www.greenend.org.uk/rjk/tech/poll.html

#+OPENMCL ; because this method doesn't exist in CCL, and the default doesn't work for udpsockets.
(defmethod ccl::local-socket-address ((socket ccl::udp-socket))
  (getf (ccl::socket-keys socket) :local-address))

#+OPENMCL
(let ((ccl::*warn-if-redefine-kernel* nil))
; note that #'stream-device is already checking that stream-ioblock is non-nil.
;  the stream-device is contained within the ioblock, so this completely
;  subsumes the old method
(defmethod open-stream-p ((stream buffered-stream-mixin))
  (not (null (stream-device stream nil))))

(defmethod open-stream-p ((stream basic-stream))
  (not (null (stream-device stream nil))))

(let ((ccl::*warn-if-redefine-kernel* nil))
  (defun unread-data-available-p (fd)
    (fd-input-available-p fd 0)))

#|
fd-input-available-p* table of returned values vs. timeout value and event occurrence.

                                       Timeout Value
  Event         |      NIL         |        0         |       >0
----------------+------------------+------------------+-----------------
Input Available | (T NIL Status)   | (T NIL Status)   | (T NIL Status)
No Input Avail  |     N/A          | (NIL NIL Status) | (NIL NIL Status)
Error           | (X Errno Status) | (X Errno Status) | (X Errno Status)

X means either T or NIL. Usually NIL, but T is conceivable.
N/A means it can't happen because the routine would not return in this case.
Errno is the system supplied error number. Always a negative integer.
Status is the revents bitmask returned from POLL.
|#

; See http://www.greenend.org.uk/rjk/tech/poll.html
(defun fd-input-available-p* (fd &optional timeout)
  "Timeout = nil: Block indefinitely.
  Timeout > 0  : Block for that number of milliseconds.
  Timeout = 0  : Return immediately.
  Output values: (successp error status)
  NOTE: not fixed for Windows version as yet)."
  (check-type timeout (or null (integer 0 *)))
  (rlet ((pollfds (:array (:struct :pollfd) 1)))
    (setf (pref (paref pollfds (:* (:struct :pollfd)) 0) :pollfd.fd) fd
          (pref (paref pollfds (:* (:struct :pollfd)) 0) :pollfd.events) #$POLLIN)
    (let* ((res (int-errno-call (#_poll pollfds 1 (or timeout -1))))
           ; res is errno if an error occurred, >0 if success, or 0 if timed out.
           ; res can never be 0 if timeout was nil. It can be 0 if timeout=0. This just means input is not available.
           (stat (pref (paref pollfds (:* (:struct :pollfd)) 0) :pollfd.revents)))
      (declare (fixnum res stat))
      #+DEBUG
      (when (and (/= stat 0)
                 (/= stat #$POLLIN))
        (format t "~% fc = ~D, stat = ~D. res = ~D" fd stat res))
      (values (> res 0) ; success
              (and (< res 0) res) ; errno
              stat))))


#|
process-input-wait* table of returned values vs. timeout value and event occurrence.

                                       Timeout Value
  Event           |      NIL         |        0         |       >0
------------------+------------------+------------------+-----------------
Input Available   | (T NIL Status)   | (T NIL Status)   | (T NIL Status)
No Input Avail    |     N/A          | (NIL NIL Status) | (NIL NIL Status)
Errno /= EINTR    | (X Errno Status) | (X Errno Status) | (X Errno Status)
Errno  = EINTR    |     N/A          | (X Errno Status) | Retry

X means either T or NIL. Usually NIL, but T is conceivable.
N/A means it can't happen because the routine would not return in this case.
Errno is the system supplied error number. Always a negative integer.
Status is the revents bitmask returned from POLL.
Retry means loop unless timeout has been exceeded, in which case return (NIL NIL Status).

Only difference between process-input-wait* and fd-input-available-p* is the former
may automatically retry on occurrence of EINTR.
|#

(defun process-input-wait* (fd &optional timeout)
  "Wait until input is available on a given file-descriptor.
  Timeout is in milliseconds.
  Timeout = nil: Block indefinitely.
  Timeout > 0  : Block for that number of milliseconds.
  Timeout = 0  : Return immediately.
  Output values: (successp errno status)"
  (check-type timeout (or null (integer 0 *)))
  (rlet ((now :timeval))
    (let* ((wait-end 
            (when (and timeout (not (zerop timeout)))
              (gettimeofday now)
              (+ (timeval->milliseconds now) timeout))))
      (loop
        (multiple-value-bind (success errno status)  ; errno is negative integer if error occurred; nil otherwise.
                             (fd-input-available-p* fd timeout)
          (when success
            (return (values success errno status)))
          (when (null errno)         ;timed out. No error.
            (return (values nil nil status)))
          (unless (eql errno (- #$EINTR))
            (return (values nil errno status)))
          ; #$EINTR happened. Retry if timeout not exceeded or no timeout given.
          (when (and timeout (zerop timeout))
            (return (values nil errno status)))
          (when wait-end
            (gettimeofday now)
            (setq timeout (- wait-end (timeval->milliseconds now)))
            (if (<= timeout 0)
                (return (values nil errno status)))))))))
)