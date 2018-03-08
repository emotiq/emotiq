;; call-path-checker.lisp -- A visual aid to determining if one function can be reached from another
;;
;; DM/RAL 08/09
;; --------------------------------------------------------------------------------
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
;; --------------------------------------------------------------------------------
;; Feel free to use, modify, or whatever...
;;
;; Use as:
;;  1) First load in your code that you want to explore
;;
;;  2) Enter:  (path-between fn1 fn2)  where path runs from fn1 to fn2
;;      and both must be dspecs for functions or methods:
;;      e.g., (path-between 'connect-to-database 'rollback)
;;        or  (path-between '(method deref (oid)) 'fetch-from-logfile)
;;
;;  3) If there are one or more paths, they will be displayed in a graph pane
;;     Otherwise, the returned value is NIL.
;; --------------------------------------------------------------------------------

(in-package :user)

(defun view-tree (from to tree)
  (when tree
    (capi:contain
     (make-instance
      'capi:graph-pane
      :title               (format nil "Path from ~A to ~A" from to)
      :roots               (list tree)
      :children-function   #'cdr
      :print-function      #'(lambda (node)
                               (format nil "~A"
                                       (let ((ds (car node)))
                                         (case (car ds)
                                           (function (cadr ds))
                                           (method   ds)))))
      :edge-pinboard-class 'capi:arrow-pinboard-object
      :interaction         :single-selection
      :action-callback     'graph-pane-action-callback
      )
     :best-width  500
     :best-height 300)))

(defun graph-pane-action-callback (node intf)
  (declare (ignore intf))
  (ignore-errors
    (ed (car node))))

(defun path-between (dspec1 dspec2)
  ;; dspec1 and dspec2 are approximate dspecs
  ;; e.g., (path-between 'deref 'fetch-from-logfile)
  ;;   or  (path-between '(method deref (oid)) 'fetch-from-logfile)
  (let ((ds1  (dspec:canonicalize-dspec dspec1))
        (ds2  (dspec:canonicalize-dspec dspec2))
        (seen (make-hash-table :test #'dspec:dspec-equal)))
    
    (labels ((dspec-type (ds)
               (case (car ds)
                 (function
                  (ignore-errors ;; produce NIL on error, which will just skip it
                    (if (typep (fdefinition (cadr ds)) 'standard-generic-function)
                        'generic-function
                      'function)))
                 (method 'method)))

             (make-method-dspec (ds meth)
               (dspec:canonicalize-dspec
                `(method ,(cadr ds)
                         ,@(method-qualifiers meth)
                         ,(mapcar (lambda (spec)
                                    (if (consp spec)
                                        spec
                                      (class-name spec)))
                                  (method-specializers meth)))))

             (build-tree (ds)
               (if (dspec:dspec-equal ds ds2)
                   (list ds)
                 (let ((found nil))
                   (setf (gethash ds seen) t)
                   (case (dspec-type ds)
                     
                     (generic-function
                      (let* ((fn      (fdefinition (cadr ds)))
                             (methods (clos:generic-function-methods fn)))
                        (dolist (meth methods)
                          (let ((meth-ds (make-method-dspec ds meth)))
                            (unless (gethash meth-ds seen)
                              (um:when-let (path (build-tree meth-ds))
                                (push path found)))))))
                     
                     ((function method)
                      (dolist (sub-fn (calls-who ds))
                        (let ((sub-ds (dspec:canonicalize-dspec sub-fn)))
                          (unless (gethash sub-ds seen)
                            (um:when-let (path (build-tree sub-ds))
                              (push path found)))))) )
                   
                   (when found
                     (cons ds (nreverse found))) ))))
      
      (view-tree dspec1 dspec2 (build-tree ds1)) )))
            
              

