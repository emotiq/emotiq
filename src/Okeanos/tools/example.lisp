
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

