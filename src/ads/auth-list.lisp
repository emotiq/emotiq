;; auth-list.lisp -- Authenticated lists
;;
;; DM/Emotiq  01/18
;; -----------------------------------------------------------
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


(in-package :ads)

(defclass auth-list ()
  ((lst  :reader  auth-list-lst
         :initarg :lst)))

(defun make-auth-list (&rest items)
  (auth (make-instance 'auth-list
                       :lst (mapcar 'auth items))))

(defmethod shallow ((alst auth-list))
  (make-instance 'auth-list
                 :lst (mapcar 'shallow (auth-list-lst alst))))
              
(defmethod fetch ((alst auth-list) (path cons))
  ;; a path here is a list of (:T :T :T :H), always ending in :H
  (labels ((iter (lst path)
             (ecase (car path)
               (:H  (fetch (car lst) (cdr path)))
               (:T  (iter (cdr lst) (cdr path)))
               )))
    (iter (auth-list-lst alst) path)))

(defmethod fetch ((alst auth-list) (path null))
  (error "Non-empty path expected"))

#|
(let* ((alst (make-auth-list :one :two :three))
       (path '(:T :H)))
  (multiple-value-bind (item wlist)
      (prove (fetch alst path))
    (verify wlist (fetch (shallow alst) path))))
 |#
