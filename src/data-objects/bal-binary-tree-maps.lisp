;; --------------------------------------------
(in-package :maps)
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

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

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))

;; --------------------------------------------

(defstruct map-cell
  key val)

(defmethod ord:compare ((a map-cell) (b map-cell))
  ;; for comparing two map cells
  ;; used by sets:add
  (ord:compare (map-cell-key a) (map-cell-key b)))

(defmethod ord:compare (a (b map-cell))
  ;; for comparing keys against map-cells
  (ord:compare a (map-cell-key b)))

(defmethod ord:compare ((a map-cell) b)
  ;; for comparing map-cells against keys
  ;; allows us to perform (set:diff map set) to remove keys
  (ord:compare (map-cell-key a) b))

;; ----------------------------------------------

(defmethod add (key val (map tree))
  (sets:add (make-map-cell
             :key key
             :val val)
            map))

(defmethod find (key (map tree) &optional default)
  ;; eval with contant stack space - S(1)
  (multiple-value-bind (found cell) (mem key map)
    (if found
        (values (map-cell-val cell) t)
      default)))

(defmethod fold (f (map tree) accu)
  ;; eval with S(Log2(N))
  (sets:fold (lambda (cell accu)
               (funcall f (map-cell-key cell) (map-cell-val cell) accu))
             map accu))

(defmethod mapi (f (map tree))
  ;; eval with S(Log2(N))
  (let ((new-map (empty)))
    (sets:iter (lambda (cell)
                 (let ((key (map-cell-key cell)))
                   (setf new-map (add key (funcall f key (map-cell-val cell)) new-map))))
            map)
    new-map))

(defmethod map (f (map tree))
  (mapi #'(lambda (k v)
            (declare (ignore k))
            (funcall f v))
        map))

(defmethod iter (f (map tree))
  (sets:iter (lambda (cell)
               (funcall f (map-cell-key cell) (map-cell-val cell)))
             map))

#|
;; test code for map
(let ((x (empty)))
  (setf x (add 'this 15 x))
  (setf x (add 'that 32 x))
  (setf x (add 'thother 64 x))
  (inspect x)
  (sets:iter #'print x)
  (find 'else x :not-found)
  (fold (lambda (k v acc)
          (cons (print (list k (+ v 3))) acc))
        x
        nil))
  |#
#||#
#+:LISPWORKS
(defmethod lispworks:get-inspector-values ((map sets:node) (mode (eql 'list-form)))
  (declare (ignore mode))
  (let* ((elts (sets:elements map)))
    (if (every #'map-cell-p elts)
        (let ((keys (mapcar #'map-cell-key elts))
              (vals (mapcar #'map-cell-val elts)))
          (values :entries (list (mapcar #'cons keys vals))))
      (values :elements (list elts)))))
#||#  
