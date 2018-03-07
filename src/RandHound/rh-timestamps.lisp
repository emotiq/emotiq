;; rh-timestamps.lisp - Randhound Timestamping
;;
;; DM/Emotiq  03/18
;; ---------------------------------------------------------------
#|
The MIT License

Copyright (c) 2018 Emotiq AG

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :randhound/common)

(defun day-name (day)
  (cdr (assoc day '((0 . "Mon")
                    (1 . "Tue")
                    (2 . "Wed")
                    (3 . "Thu")
                    (4 . "Fri")
                    (5 . "Sat")
                    (6 . "Sun")))))

(defun month-name (mon)
  (cdr (assoc mon '(( 1 . "Jan")
                    ( 2 . "Feb")
                    ( 3 . "Mar")
                    ( 4 . "Apr")
                    ( 5 . "May")
                    ( 6 . "Jun")
                    ( 7 . "Jul")
                    ( 8 . "Aug")
                    ( 9 . "Sep")
                    (10 . "Oct")
                    (11 . "Nov")
                    (12 . "Dec")))))

(defun format-timestamp (time)
  (multiple-value-bind (sec min hr date mon yr day) (decode-universal-time time)
    (format nil "~A ~2,'0d ~A ~d  ~2,'0d:~2,'0d:~2,'0d"
            (day-name day)
            date (month-name mon) yr
            hr min sec)))
    
(defun get-timestamp ()
  (let ((now (get-universal-time)))
    (values (format-timestamp now)
            now))) ;; return numeric value in case we need ordering


