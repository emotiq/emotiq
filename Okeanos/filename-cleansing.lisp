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

(defconstant *spchars* "\\*:")

(defmethod translate-to-filename ((sym symbol))
  (translate-to-filename (symbol-name sym)))

(defmethod translate-to-filename ((str string))
  (with-output-to-string (d)
    (map nil #'(lambda (c)
               (if (find c *spchars*)
                   (format d "~2,'0D" (char-code c))
                 (write-char c d)))
         str)))

#|
(setf str (symbol-name '|12~`!@#$%^&*()_+=-][}{'":;/?>.<,|\|))
(translate-to-filename str)
(with-open-file (f (translate-to-filename "<diddly.x>") ;; (translate-to-filename str)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
  (print :Hello! f))

(mapcar (compose #'translate-to-filename 'package-name) (list-all-packages))
|#

