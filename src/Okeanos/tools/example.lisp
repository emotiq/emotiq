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


;; Create a new list and mark it as persistent
(setf x '(1 2 3)) 
(persist x)

;; get a Ref Cell to the list
(setf xref (ref x))

;; now commit to the database
(commit)

;; At this point the list has been written to the database.
;; The binding to x still exists, but a copy of the object
;; will be retrieved from the Ref Cell...

(let y (deref xref))

(defclass address-entry ()
  ((name   :accessor ae-name
           :indexed  :unique
           :initarg  :name)

   (location :accessor ae-location
             :indexed  t
             :initarg  :location))
  
  (:metaclass persistent-class))


(defclass extended-address-entry (address-entry)
  ((telno  :accessor ae-telno
           :initarg  :telephone))

  (:metaclass persistent-class))

(make-instance 'address-entry
               :name  "Dave"
               :location "Tucson")

(make-instance 'extended-address-entry
               :name  "Dave's Lab"
               :location "Tucson"
               :telephone 123-4567)

