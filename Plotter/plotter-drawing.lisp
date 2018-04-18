
(in-package :plotter)

;; ------------------------------------------
(defun draw-path (port &rest positions)
  (gp:draw-polygon port
                   (mapcan #'append positions)))

;; ------------------------------------------
#|
(defun zip (&rest seqs)
  (apply #'map 'list #'list seqs))

(defun staircase (xv yv)
  (let ((pairs (zip xv yv)))
    (um:foldl
     (lambda (ans pair)
       (destructuring-bind (x y) pair
         (destructuring-bind (xprev yprev &rest _) ans
           (declare (ignore _))
           (let ((xmid (* 0.5 (+ x xprev))))
             (nconc (list x y xmid y xmid yprev) ans)
             ))))
     (first pairs)
     (rest pairs)
     )))

(defun make-bars (xv yv)
  (let ((pairs (zip xv yv)))
    (um:foldl
     (lambda (ans pair)
       (destructuring-bind (x y) pair
         (destructuring-bind (xprev &rest _) ans
           (declare (ignore _))
           (let ((xmid (* 0.5 (+ x xprev))))
             (nconc (list x y xmid y) ans)
             ))))
     (first pairs)
     (rest pairs)
     )))

(defun interleave (&rest seqs)
  (mapcan #'nconc (apply #'zip seqs)))

|#
;; -------------------------------------------------------
(defmethod draw-vertical-bars (port (bars <pair-scanner>))
  (let* (xprev
         yprev
         last-x
         (wd   (* 0.1 (gp:port-width port))) ;; default if only one data point
         (wd/2 (* 0.5 wd)))
    (with-pairs (bars x y)
      (when xprev
        (setf wd   (abs (- x xprev))
              wd/2 (* 0.5 wd))
        (unless (= y yprev)
          (let ((next-x (+ xprev wd/2))
                (prev-x (or last-x
                            (- xprev wd/2))
                        ))
            (gp:draw-rectangle port prev-x 0 (- next-x prev-x) yprev :filled t)
            (setf last-x next-x)
            )))
      (setf xprev x
            yprev y))
    (when xprev
      ;; use the last known width
      (let ((next-x (+ xprev wd/2))
            (prev-x (or last-x
                        (- xprev wd/2))
                    ))
        (gp:draw-rectangle port prev-x 0 (- next-x prev-x) yprev :filled t)
        ))
    ))
                             
(defmethod draw-horizontal-bars (port (bars <pair-scanner>))
  (let* (xprev
         yprev
         last-y
         (wd   (* 0.1 (gp:port-height port))) ;; default if only one data point
         (wd/2 (* 0.5 wd)))
    (with-pairs (bars x y)
      (when yprev
        (setf wd   (abs (- y yprev))
              wd/2 (* 0.5 wd))
        (unless (= x xprev)
          (let ((next-y (+ yprev wd/2))
                (prev-y (or last-y
                            (- yprev wd/2))
                        ))
            (gp:draw-rectangle port 0 prev-y xprev (- next-y prev-y) :filled t)
            (setf last-y next-y)
            )))
      (setf xprev x
            yprev y))
    (when xprev
      ;; use the last known width
      (let ((next-y (+ yprev wd/2))
            (prev-y (or last-y
                        (- yprev wd/2))
                    ))
        (gp:draw-rectangle port 0 prev-y xprev (- next-y prev-y) :filled t)
        ))
    ))

(defmethod draw-staircase (port (pairs <pair-scanner>))
  (let* (xprev
         yprev
         last-x
         (wd    (* 0.1 (gp:port-width port)))     ;; default for only one data point
         (wd/2  (* 0.5 wd)))
    (with-pairs (pairs x y)
      (when xprev
        (setf wd   (abs (- x xprev))
              wd/2 (* 0.5 wd))
        (unless (= y yprev)
          (let ((next-x (- x wd/2)))
            (gp:draw-polygon port
                             (list (or last-x (- xprev wd/2)) yprev
                                   next-x yprev
                                   next-x y)
                             :closed nil)
            (setf last-x next-x)
            )))
      (setf xprev x
            yprev y))
    (when xprev
      (gp:draw-line port
                    (or last-x (- xprev wd/2)) yprev
                    (+ xprev wd/2) yprev))
          ))

(defmethod draw-polyline (port (pairs <pair-scanner>))
  (let (xprev yprev)
    (with-pairs (pairs x y)
      (when (and xprev
                 (or (/= x xprev)
                     (/= y yprev)))
        (gp:draw-line port xprev yprev x y))
      (setf xprev x
            yprev y))
    ))
  
;; ----------------------------------------------------------  

(defun get-symbol-plotfn (port sf symbol-style)
  (labels ((draw-symbol (fn)
             #-:WIN32
             (with-color (port (or (if (fill-color symbol-style)
                                     (adjust-color port
                                                   (fill-color symbol-style)
                                                   (fill-alpha symbol-style)))
                                   #.(color:make-gray 1.0 0.25)))
               (funcall fn t))
             #+:WIN32
             (when (fill-color symbol-style)
               (with-color (port (adjust-color port
                                               (fill-color symbol-style)
                                               (fill-alpha symbol-style)))
                 (funcall fn t)))
             (gp:with-graphics-state (port
                                      :thickness  (adjust-linewidth (* sf (border-thick symbol-style)))
                                      :foreground (adjust-color port
                                                                (border-color symbol-style)
                                                                (border-alpha symbol-style)))
               (funcall fn))))
    
    (ecase (plot-symbol symbol-style)
      (:cross     (lambda (x y)
                    (gp:with-graphics-state (port
                                             :thickness  (adjust-linewidth (* sf (border-thick symbol-style)))
                                             :foreground (adjust-color port
                                                                       (border-color symbol-style)
                                                                       (border-alpha symbol-style)))
                      (gp:draw-line port (- x 3) y (+ x 3) y)
                      (gp:draw-line port x (- y 3) x (+ y 3))
                      )))
      
      (:x         (lambda (x y)
                    (gp:with-graphics-state (port
                                             :thickness  (adjust-linewidth (* sf (border-thick symbol-style)))
                                             :foreground (adjust-color port
                                                                       (border-color symbol-style)
                                                                       (border-alpha symbol-style)))
                      (gp:draw-line port (- x 3) (- y 3) (+ x 3) (+ y 3))
                      (gp:draw-line port (+ x 3) (- y 3) (- x 3) (+ y 3))
                      )))
      
      ((:circle :sampled-data)
       (lambda (x y)
         (labels ((draw-circle (&optional filled)
                    (gp:draw-circle port
                                    x 
                                    #-:WIN32 (- y 0.5)
                                    #+:WIN32 y
                                    3
                                    :filled filled)))
           (draw-symbol #'draw-circle)
           )))

      ((:box :square)
       (lambda (x y)
         (labels ((draw-rectangle (&optional filled)
                    (gp:draw-rectangle port (- x 3) (- y 3) 6 6
                                       :filled filled)))
           (draw-symbol #'draw-rectangle)
           )))

      ((:triangle :up-triangle)
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon port
                                     (list (- x 3) (+ y 3)
                                           x (- y 4)
                                           (+ x 3) (+ y 3))
                                     :closed t
                                     :filled filled)))
           (draw-symbol #'draw-triangle)
           )))
      
      (:down-triangle
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon port
                                     (list (- x 3) (- y 3)
                                           x (+ y 4)
                                           (+ x 3) (- y 3))
                                     :closed t
                                     :filled filled)))
           (draw-symbol #'draw-triangle)
           )))
      
      (:right-triangle
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon port
                                     (list (- x 3) (- y 3)
                                           (+ x 4) y
                                           (- x 3) (+ y 3))
                                     :closed t
                                     :filled filled)))
           (draw-symbol #'draw-triangle)
           )))
      
      (:left-triangle
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon port
                                     (list (+ x 3) (- y 3)
                                           (- x 4) y
                                           (+ x 3) (+ y 3))
                                     :closed t
                                     :filled filled)))
           (draw-symbol #'draw-triangle)
           )))

      (:dot
       (lambda (x y)
         (with-color (port (adjust-color port
                                         (border-color symbol-style)
                                         (border-alpha symbol-style)))
           (gp:draw-circle port x (1- y) 0.5))
         ))
      )))


(defmethod unsafe-pw-plot-xv-yv ((cpw <plotter-mixin>) port xvector yvector 
                          &key
                          ;; (color #.(color:make-rgb 0.0 0.5 0.0))
                          ;; alpha
                          ;; thick
                          ;; (linewidth (or thick 1))
                          ;; linedashing
                          ;; symbol
                          ;; plot-joined
                          ;; legend
                          legend-x
                          legend-y
                          legend-anchor
                          ;; (border-color color)
                          ;; symbol-filled
                          ;; (fill-color color)
                          ;; (border-thick linewidth)
                          ;; barwidth
                          ;; bar-offset
                          plot-style
                          &allow-other-keys)
  ;; this is the base plotting routine
  ;; called only from within the pane process
  (let* ((sf        (plotter-sf  cpw))
         (box       (let ((box (plotter-box cpw)))
                      (adjust-box
                       (list (1+ (* sf (box-left box)))
                             (* sf (box-top box))
                             (1- (* sf (box-width box)))
                             (* sf (box-height box))))
                      ))
         (xform     (plotter-xform cpw))
         
         ;; (color     (adjust-color cpw color alpha))
         ;; (linewidth (adjust-linewidth (* sf linewidth)))
         (line-style   (line-style   plot-style))
         (symbol-style (symbol-style plot-style))

         (nel       (if xvector
                        (min (length-of xvector) (length-of yvector))
                      (length-of yvector)))

         (xs         (let ((scanner (make-scanner (or xvector
                                                      nel))
                                    ))
                       (if (plotter-xlog cpw)
                           (make-transformer scanner #'log10)
                         scanner)))

         (ys         (let ((scanner (make-scanner yvector)))
                       (if (plotter-ylog cpw)
                           (make-transformer scanner #'log10)
                         scanner)))
         (pairs     (make-pair-scanner xs ys)))

    (when (legend plot-style)
      (append-legend cpw plot-style))

    (when legend-x
      (setf (plotter-legend-x cpw) legend-x))
    (when legend-y
      (setf (plotter-legend-y cpw) legend-y))
    (when legend-anchor
      (setf (plotter-legend-anchor cpw) legend-anchor))

    (gp:with-graphics-state (port
                             ;; :thickness  linewidth
                             ;; :dashed     (not (null linedashing))
                             ;; :dash       (mapcar (um:expanded-curry (v) #'* sf) linedashing)
                             ;; :foreground color
                             :line-end-style   :butt
                             :line-joint-style :miter
                             :scale-thickness  NIL
                             :mask       box)

      (labels ((draw-lines ()
                 (gp:with-graphics-state (port
                                          :thickness  (adjust-linewidth (* sf (line-thick line-style)))
                                          :foreground (adjust-color port
                                                                    (line-color line-style)
                                                                    (line-alpha line-style))
                                          :dashed     (line-dashing line-style)
                                          :dash       (mapcar (um:expanded-curry (v) #'* sf)
                                                              (line-dashing line-style)))
                   (gp:with-graphics-scale (port sf sf)
                     (gp:with-graphics-transform (port xform)
                       (draw-polyline port pairs)))
                   )))

        (cond (symbol-style
               (case (plot-symbol symbol-style)
                 
                 (:vbars
                  (gp:with-graphics-state (port
                                           :foreground (adjust-color port
                                                                     (fill-color symbol-style)
                                                                     (fill-alpha symbol-style)))
                    (gp:with-graphics-scale (port sf sf)
                      (if (bar-width symbol-style)
                        (let* ((wd   (get-x-width cpw (bar-width symbol-style)))
                               (wd/2 (* 0.5 wd))
                               (off  (if (bar-offset symbol-style)
                                       (get-x-width cpw (bar-offset symbol-style))
                                       0)))
                          (with-pairs (pairs x y)
                            (multiple-value-bind (xx yy)
                                (gp:transform-point xform x y)
                              (multiple-value-bind (_ yy0)
                                  (gp:transform-point xform x 0)
                                (declare (ignore _))
                                (gp:draw-rectangle port
                                                   (+ off (- xx wd/2)) yy0
                                                   wd (- yy yy0)
                                                   :filled t)
                                ))))
                        (gp:with-graphics-transform (port xform)
                          (draw-vertical-bars port pairs))
                        ))))
                 
                 (:hbars
                  (gp:with-graphics-state (port
                                           :foreground (adjust-color port
                                                                     (fill-color symbol-style)
                                                                     (fill-alpha symbol-style)))
                    (gp:with-graphics-scale (port sf sf)
                      (if (bar-width symbol-style)
                        (let* ((wd   (get-y-width cpw (bar-width symbol-style)))
                               (wd/2 (* 0.5 wd))
                               (off  (if (bar-offset symbol-style)
                                       (get-y-width cpw (bar-offset symbol-style))
                                       0)))
                          (with-pairs (pairs x y)
                            (multiple-value-bind (xx yy)
                                (gp:transform-point xform x y)
                              (multiple-value-bind (xx0 _)
                                  (gp:transform-point xform 0 y)
                                (declare (ignore _))
                                (gp:draw-rectangle port
                                                   xx0 (+ off (- yy wd/2))
                                                   (- xx xx0) wd
                                                   :filled t)
                                ))))
                        (gp:with-graphics-transform (port xform)
                          (draw-horizontal-bars port pairs))
                        ))))
                 
                 (:sampled-data
                  (gp:with-graphics-state (port
                                           :foreground (adjust-color port
                                                                     (or (and line-style
                                                                              (line-color line-style))
                                                                         :black)
                                                                     (or (and line-style
                                                                              (line-alpha line-style))
                                                                         1))
                                           :thickness  (adjust-linewidth (* sf (or (and line-style
                                                                                        (line-thick line-style))
                                                                                   1))))
                    (gp:with-graphics-scale (port sf sf)
                      (let ((dotfn (get-symbol-plotfn port sf (symbol-style plot-style))))
                        (with-pairs (pairs x y)
                          (multiple-value-bind (xx yy)
                              (gp:transform-point xform x y)
                            (multiple-value-bind (_ yy0)
                                (gp:transform-point xform x 0)
                              (declare (ignore _))
                              (gp:draw-line port xx yy0 xx yy)
                              (funcall dotfn xx yy))
                            ))
                        ))))
                 
                 (otherwise
                  (when line-style
                      (draw-lines))
                  
                  (gp:with-graphics-scale (port sf sf)
                    (let ((plotfn (get-symbol-plotfn port sf symbol-style)))
                      (with-pairs (pairs x y)
                        (multiple-value-bind (xx yy)
                            (gp:transform-point xform x y)
                          (funcall plotfn xx yy)
                          )) )))
                 ))
              
              (line-style
               (case (line-type line-style)
                 (:stepped
                  (gp:with-graphics-state (port
                                           :thickness  (adjust-linewidth (* sf (line-thick line-style)))
                                           :foreground (adjust-color port
                                                                     (line-color line-style)
                                                                     (line-alpha line-style)))
                    (gp:with-graphics-scale (port sf sf)
                      (gp:with-graphics-transform (port xform)
                        (draw-staircase port pairs)))
                    ))
                 
                 (otherwise ;; :interpolated
                            (draw-lines))
                 )))
        ))))

(defmethod pw-plot-xv-yv ((cpw <plotter-mixin>) port xvector yvector &rest args)
  (progn ;; ignore-errors
    (apply #'unsafe-pw-plot-xv-yv cpw port xvector yvector args)))

;; ------------------------------------------------------------------------------
(defun get-bar-symbol-plotfn (port symbol color neg-color bar-width testfn)
  ;; bear in mind that the y values at this point are absolute screen
  ;; coords and are inverted with respect to data ordering
  (ecase symbol
    (:sigma
     (lambda (x ys)
       (destructure-vector (ymin ymax) ys
         (gp:draw-line port x ymin x ymax)
         (gp:draw-line port (- x (/ bar-width 2)) ymin (+ x (/ bar-width 2)) ymin)
         (gp:draw-line port (- x (/ bar-width 2)) ymax (+ x (/ bar-width 2)) ymax)
         )))

    (:hl-bar
     (lambda (x ys)
       (destructure-vector (ymin ymax) ys
         (gp:draw-line port x ymin x ymax)
         )))
    
    (:hlc-bar
     (lambda (x ys)
       (destructure-vector (h l c) ys
         (gp:draw-line port x l x h)
         (gp:draw-line port x c (+ x (/ bar-width 2)) c)
         )))
    
    (:ohlc-bar
     (lambda (x ys)
       (destructure-vector (o h l c) ys
         (with-color (port (if (funcall testfn c o) neg-color color))
           (gp:draw-line port x l x h)
           (gp:draw-line port (- x (/ bar-width 2)) o x o)
           (gp:draw-line port x c (+ x (/ bar-width 2)) c)
           ))))
    
    (:candlestick
     (lambda (x ys)
       (destructure-vector (o h l c) ys
         (if (funcall testfn c o)
             (with-color (port neg-color)
               (gp:draw-line port x l x h)
               (gp:draw-rectangle port (- x (/ bar-width 2)) o bar-width (- c o)
                                  :filled t))
           (progn
             (with-color (port :black)
               (gp:draw-line port x l x h))
             (with-color (port color)
               (gp:draw-rectangle port (- x (/ bar-width 2)) o bar-width (- c o)
                                  :filled t))
             (with-color (port :black)
               (gp:draw-rectangle port (- x (/ bar-width 2)) o bar-width (- c o)))
             ))
         )))
    ))

;;-------------------------------------------------------------------
(defmethod unsafe-pw-plot-bars-xv-yv ((cpw <plotter-mixin>) port xvector yvectors 
                          &key
                          (color #.(color:make-rgb 0.0 0.5 0.0))
                          (neg-color color)
                          alpha
                          thick
                          (linewidth (or thick 1))
                          (bar-width 6)
                          (symbol (ecase (length yvectors)
                                    (2 :sigma)
                                    (3 :hlc-bar)
                                    (4 :ohlc-bar)))
                          &allow-other-keys)
  ;; this is the base bar-plotting routine
  ;; called only from within the pane process
  (let* ((sf        (plotter-sf  cpw))
         (box       (let ((box (plotter-box cpw)))
                      (adjust-box
                       (list (1+ (* sf (box-left box)))
                             (* sf (box-top box))
                             (1- (* sf (box-width box)))
                             (* sf (box-height box))))
                      ))
         (xform     (plotter-xform cpw))
         (color     (adjust-color cpw color alpha))
         (neg-color (adjust-color cpw neg-color alpha))
         (linewidth (adjust-linewidth (* sf linewidth)))

         (nel       (let ((nely (reduce #'min (mapcar #'length-of yvectors))))
                      (if xvector
                          (min (length-of xvector) nely)
                        nely)))
         
         (xs        (let* ((xform   (lambda (x)
                                      (gp:transform-point xform x 0)))
                           (scanner (make-scanner (or xvector
                                                      nel)
                                                  :max-items nel)))
                      (make-transformer scanner
                                        (if (plotter-xlog cpw)
                                            (um:compose xform #'log10)
                                          xform))
                      ))

         (xform-y   (lambda (y)
                      (second (multiple-value-list
                               (gp:transform-point xform 0 y)))
                      ))

         (ys        (let* ((scanners (mapcar #'make-scanner yvectors)))
                      (mapcar (um:rcurry #'make-transformer
                                         (if (plotter-ylog cpw)
                                             (um:compose xform-y #'log10)
                                           xform-y))
                              scanners)
                      ))
         (c<o-testfn (let ((y1 (funcall xform-y 0))
                           (y2 (funcall xform-y 1)))
                       (if (< y2 y1)
                           #'>
                         #'<)))
         (plotfn (get-bar-symbol-plotfn port symbol
                                        color neg-color bar-width
                                        c<o-testfn))
         (tmp       (make-array (length ys))))
    
    (gp:with-graphics-state (port
                             :thickness  linewidth
                             :foreground color
                             :line-end-style   :butt
                             :line-joint-style :miter
                             :mask       box)
      
      (gp:with-graphics-scale (port sf sf)
        (with-scanner (xs x)
          (map-into tmp #'next-item ys)
          (funcall plotfn x tmp))))
    ))

(defmethod pw-plot-bars-xv-yv ((cpw <plotter-mixin>) port xvector yvectors &rest args)
  (progn ;; ignore-errors
    (apply #'unsafe-pw-plot-bars-xv-yv cpw port xvector yvectors args)))

;; ============================================================
(defun plt-draw-shape (pane port shape x0 y0 x1 y1
                          &key
                          color alpha filled
                          border-thick border-color border-alpha
                          start-angle sweep-angle)
  ;; for rectangles: shape = :rect, (x0,y0) and (x1,y1) are opposite corners
  ;; for ellipses:   shape = :ellipse, (x0,y0) is ctr (x1,y1) are radii
  (let* ((x0        (get-x-for-location pane (get-x-location pane x0)))
         (y0        (get-y-for-location pane (get-y-location pane y0)))
         (x1        (get-x-for-location pane (get-x-location pane x1)))
         (y1        (get-y-for-location pane (get-y-location pane y1)))
         (x0        (if (plotter-xlog pane)
                        (log10 x0)
                      x0))
         (x1        (if (plotter-xlog pane)
                        (log10 x1)
                      x1))
         (y0        (if (plotter-ylog pane)
                        (log10 y0)
                      y0))
         (y1        (if (plotter-ylog pane)
                        (log10 y1)
                      y1))
         (wd        (- x1 x0))
         (ht        (- y1 y0))
         (sf        (plotter-sf  pane))
         (box       (let ((box (plotter-box pane)))
                      (adjust-box
                       (list (1+ (* sf (box-left box)))
                             (* sf (box-top box))
                             (1- (* sf (box-width box)))
                             (* sf (box-height box))))
                      ))
         (xform     (plotter-xform pane))
         (color     (adjust-color pane color alpha))
         (bcolor    (adjust-color pane border-color border-alpha))
         (linewidth (adjust-linewidth (* sf (or border-thick 0)))))
    
    (gp:with-graphics-state (port
                             :thickness  linewidth
                             :foreground color
                             :line-end-style   :butt
                             :line-joint-style :miter
                             :mask       box)

      (gp:with-graphics-scale (port sf sf)

        (gp:with-graphics-transform (port xform)
          (when filled
            (ecase shape
              (:rect
               (gp:draw-rectangle port
                                  x0 y0 wd ht
                                  :filled t))
              (:ellipse
               (gp:draw-ellipse port
                                x0 y0 x1 y1
                                :filled t))
              
              (:arc
               (gp:draw-arc port
                            x0 y0 wd ht
                            start-angle sweep-angle
                            :filled t))
              ))
          
          (when border-thick
            (with-color (port bcolor)
              (case shape
                (:rect
                 (gp:draw-rectangle port
                                    x0 y0 wd ht
                                    :filled nil))
                (:ellipse
                 (gp:draw-ellipse port
                                  x0 y0 x1 y1
                                  :filled nil))

                (:arc
                 (gp:draw-arc port
                              x0 y0 wd ht
                              start-angle sweep-angle
                              :filled nil))
                )))
          )))
    ))


