;; rollback.lisp -- transaction rollback for persistent objects
;; --------------------------------------------------------------------------------------
;;
;; DM/RAL  08/08
;; --------------------------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2008 Refined Audiometrics Laboratory, LLC

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
;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

;; -------------------------------

(defun rollback (&optional (newstate nil newstate-p))
  (let ((highest-wts (get-new-ts)))

    (labels ((vote (ts)
               (if (uuid:uuid< highest-wts ts)
                   (setf highest-wts ts))))
      
      (when *current-connection*
        
        ;; cull out the databases that are no longer available
        (deletef-if (cnx-info-mappings *current-connection*)
                    (complement (compose (rcurry 'member *db-files*)
                                         'car)))
        
        (do-databases
         #'(lambda (triple)
           (clear-keys-values (cadr triple))
           (clear-keys-values (cddr triple))))
        
        (let ((tags nil))
          (with-databases ()
            (if-remote
             (push (list (rpc-send `(rollback :QUERY))) tags)
             ;; else
             (vote (get-last-ts))))

          (dolist (reply (check-replies tags))
            (vote (cdr reply))) ))

      (cond ((and newstate-p
                  (null newstate))
             (setf (cnx-info-db-state *current-connection*) nil))
            
            ((eq newstate :QUERY) highest-wts)
            
            ((not (remote-access-p))
             (set-timestamp highest-wts))
            ))))

(defun set-timestamp (wts)
  (setf (cnx-info-db-state *current-connection*) wts)
  (with-databases ()
    (when-remote
      (rpc `(set-timestamp ,wts)) )))
       
;; -------------------------------

(define-condition rollback-exception ()
  ())

(defun raise-rollback-exception ()
  (rollback)
  (error (make-condition 'rollback-exception)))

(define-condition serious-rollback-exception (error)
  ((msg  :reader  rbe-message
         :initarg :message)
   (args :reader  rbe-args
         :initarg :args
         :initform nil))
  (:report report-rbe-exn))

(defun report-rbe-exn (err stream)
  (apply 'format stream (rbe-message err) (rbe-args err)))

(defun raise-serious-rollback-exception (msg &rest args)
  (rollback)
  (error (make-condition 'serious-rollback-exception
                         :message msg
                         :args    args)))

        
        
