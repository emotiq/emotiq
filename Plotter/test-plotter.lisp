
(in-package "PLOTTER")

(capi:define-interface pw4 ()
  ()
  (:panes
   (p1 <plotter-pane>
       :accessor p1)
   (p2 <plotter-pane>
       :accessor p2)
   (p3 <plotter-pane>
       :accessor p3)
   (p4 <plotter-pane>
       :accessor p4))
  (:layouts
   (default-layout
    capi:grid-layout
    '(p1 p2 p3 p4)
    :columns 2))
  (:default-initargs
   :layout 'default-layout
   :window-styles '(:internal-borderless))
  )

(setf intf (capi:display
            (make-instance 'pw4)))

(defun sinc (x)
  (/ (sin x) x))

(clear (p1 intf))
(fplot (p1 intf) '(-20 20) #'sinc :thick 2 :color :darkgreen :title "Sinc")
(fplot (p4 intf) '(-20 0) #'sinc :thick 2 :color :orange :title "Sinc")

;; -------------------------------
(setf px (capi:contain
          (make-instance '<plotter-pane>)))
(fplot px '(-20 0) #'sinc :thick 2 :color :orange :title "Sinc")


;; -------------------------------
(capi:define-interface pw4 ()
  ()
  (:panes
   (p1 <plotter-pane>
       :accessor p1)
   (p2 <plotter-pane>
       :accessor p2)
   (p3 <plotter-pane>
       :accessor p3)
   (p4 <plotter-pane>
       :accessor p4))
  (:layouts
   (default-layout
    capi:pinboard-layout
    '(p1 p2 p3 p4)))
  (:default-initargs
   :layout 'default-layout
   :window-styles '(:internal-borderless))
  )

(setf intf (capi:display
            (make-instance 'pw4)))

