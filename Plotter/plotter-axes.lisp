
(in-package :plotter)

;; ------------------------------------------
(defun calc-starting-sf (vmin vmax)
  (let ((diff (abs (- vmax vmin))))
    ;; return a starting scale factor so that the range between vmin and vmax
    ;; is greater than 1 but less than 10
    (expt 10 (- (floor (log diff 10))))
    ))

(defun adjust-sf-for-midvalue (sf vmin vmax)
  ;; adjust the scale factor sf, and find the central starting value c
  ;; such that the scaled range between sf*vmin and sf*vmax is >= 5
  ;; and such that c lies between sf*vmin and sf*vmax, and c is a multiple of 10.
  ;;
  ;; NOTE: it is possible that vmin > vmax if the user requests it.
  (let ((vmn (min vmin vmax))
        (vmx (max vmin vmax)))
    (labels ((iterz (sf)
               (if (>= (* sf (- vmx vmn) 5))
                   (list sf 0)
                 (iterz (* 10 sf))))
             (iternz (sf)
               (let ((c (* 10 (ceiling (* sf vmn) 10))))
                 (if (and (<= (* sf vmn) c (* sf vmx))
                          (>= (* sf (- vmx vmn)) 5))
                     (list sf c)
                   (iternz (* 10 sf))
                   ))
               ))
      (if (<= vmn 0 vmx)
          ;; zero is a preferred value for c
          (iterz sf)
        (iternz sf))
      )))

(defun get-delta-sf (vmin vmax)
  (adjust-sf-for-midvalue (calc-starting-sf vmin vmax) vmin vmax))

(defun calc-start-delta (vmin vmax)
  ;; compute a good axis increment and starting value
  ;; these are considered good if the increment is a decade multiple of 1, 2, or 5.
  ;; The starting value must be the largest whole part of the axis values
  ;; in one of these good increments:
  ;; e.g.,
  ;; if the axis ranges from 1.23 to 3.28, then the largest whole part will be 2.00.
  ;; That will be our starting label, and we then number by (non-overlapping strings)
  ;; at increment spacings on either side of that largest whole part.
  ;;
  ;; This avoid bizarre labels like 1.23 ... 1.37 ... 2.45 ...
  ;; giving instead, someting like  1.2 .. 1.6 .. 2.0 .. 2.4 ...
  ;; which is enormously more readable than what most plotting packages produce.
  ;; (This is the way a human would chart the axes)
  ;;
  (destructuring-bind (sf c)
      (get-delta-sf vmin vmax)
    #|
      ;; sf starting = 1/(10^Ceil( log10( Max(|vmin|, |vmax|)))),
      ;; then 10*sf -> sf
      ;; until |sf*vmax - sf*vmin| > 1.
      ;;
      ;; starting value is guaranteed to scale these two (|vmax|, |vmin|) to values
      ;; less than 1. We keep increasing the scale factor by 10 until the difference
      ;; between them, |sf*vmax - sf*vmin| > 1.
      ;;
      ;; We might not have to increase sf by 10,
      ;; e.g., vmin = -5.1, vmax = 6 ==> sf = 1/10, diff = 1.11
      ;;
      ;; Initial scaled span is: 0 <= diff <= 2,
      ;; the min case if vmin = vmax,
      ;; the max case if vmin = -1, vmax = 1.
      ;;
      (loop for sf = (/ (pow10
                         (ceiling (log10 (max (abs vmin)
                                              (abs vmax))
                                         ))
                         ))
            then (* 10.0d0 sf)
            do
            ;;
            ;; this loop finds the scale factor sf and minimum integer value c such that
            ;; the scaled min and max values span a range greater than 1
            ;; and c is no further from the scaled min value than that range.
            ;; It is the case that a <= c <= b, where a and b are the scaled min and max values,
            ;; and abs(c) is some integer multiple (positive, zero, or negative) of 10.
            ;;
            (let* ((a   (* sf vmin))
                   (b   (* sf vmax))
                   (rng (abs (- b a)))
                   (c   (* 10.0d0 (ceiling (min a b) 10.0d0))))
              (if (and (> rng 1.0d0)
                       (<= (abs (- c a)) rng))
                  (return (list sf c)))
              ))
      |#
    
    (loop for sf2 = 1.0d0 then (* 0.1d0 sf2)
          do
          (let* ((a   (* sf sf2 vmin))
                 (b   (* sf sf2 vmax))
                 (c   (* sf2 c))
                 (rng (abs (- b a))))
            
            (if (<= rng 10.0d0)
                (let* ((dv  (cond ((> rng 5.0d0) 1.0d0)
                                  ((> rng 2.0d0) 0.5d0)
                                  (t             0.2d0)))
                       (nl  (floor (abs (- c a)) dv))
                       (nu  (floor (abs (- b c)) dv))
                       (v0  (if (not (plusp (* a b)))
                                0.0d0
                              (/ c sf sf2)))
                       (dv  (/ dv sf sf2)))
                  (return (list v0 dv nl nu)))
              ))
          )))

;; ------------------------------------------
(defparameter *log-subdivs*
  (mapcar #'log10
          '(0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9
                2 3 4 5 6 7 8 9)))

(defparameter $axis-style
  (make-instance '<plot-style>
                 :line-style (make-instance '<line-style>
                                            :color #.(color:make-gray 0.5))))

(defun trim-mantissa (v)
  (string-right-trim
   "."
   (string-right-trim
    "0" v)))

(defun plabel (val)
  (if (or (zerop val)
          (and (<= 0.01 (abs val))
               (< (abs val) 10000)))
    (trim-mantissa (format nil "~,3F" (float val 1.0)))

    ;; Engineering notation
    (let* ((pwr (* 3 (floor (log (abs val) 10) 3)))
           (v   (/ val (expt 10 pwr))))
      (when (>= (round (abs v)) 1000)
        (incf pwr 3)
        (setf v (* 0.001 v)))
      (concatenate 'string
                   (trim-mantissa (format nil "~,2F" v))
                   (format nil "e~d" pwr))
      )))

(defmethod pw-axes ((cpw <plotter-mixin>) port
                    &key
                    (fullgrid t)
                    (xtitle "X")
                    (ytitle "Y")
                    (title  "Plot")
                    (axes t)
                    (axis-values t)
                    (x-axis-values t)
                    (y-axis-values t)
                    (watermarkfn #'watermark)
                    (logo *ext-logo*)
                    (logo-alpha *ext-logo-alpha*)
                    (cright1 *cright1*)
                    (cright2 *cright2*)
                    (clear t)
                    x-values
                    &allow-other-keys)
  (let* ((box   (plotter-box cpw))
         (sf    (plotter-sf cpw))
         (font  (find-best-font cpw
                                :size (* sf $normal-times-font-size)
                                ))
         (xlog  (plotter-xlog cpw))
         (ylog  (plotter-ylog cpw)))
    
    (labels
        ((qxlog (x)
           (if xlog (log10 x) x))
         (qylog (y)
           (if ylog (log10 y) y))
         (iqxlog (x)
           (if xlog (pow10 x) x))
         (iqylog (y)
           (if ylog (pow10 y) y)))

      (when clear
        (gp:clear-graphics-port port)
        (if watermarkfn
          (funcall watermarkfn cpw port logo logo-alpha
                   cright1 cright2)))

      (when title
        (draw-string-x-y cpw port title
                         (floor (* sf (+ (box-left box) (box-right box))) 2)
                         0
                         :x-alignment :center
                         :y-alignment :top
                         :font        (find-best-font cpw
                                                      :size (* sf $big-times-font-size))
                         ))

      (when axes
        (gp:with-graphics-scale (port sf sf)
          (gp:with-graphics-state (port :scale-thickness t)
            (draw-path port
                       (box-top-left     box)
                       (box-bottom-left  box)
                       (box-bottom-right box)
                       )))
        
        (when (and axis-values
                   y-axis-values)
          (pw-plot-xv-yv cpw port
                         (vector (iqxlog (plotter-xmin cpw))
                                 (iqxlog (plotter-xmax cpw)))
                         (vector (iqylog 0) (iqylog 0))
                         :plot-style $axis-style))
        
        (when (and axis-values
                   x-axis-values)
          (pw-plot-xv-yv cpw port
                         (vector (iqxlog 0) (iqxlog 0))
                         (vector (iqylog (plotter-ymin cpw))
                                 (iqylog (plotter-ymax cpw)))
                         :plot-style $axis-style)))

      (when xtitle
        (draw-string-x-y cpw port xtitle
                         (floor (* sf (+ (box-left box) (box-right box))) 2)
                         (* sf (+ (box-bottom box) (if axis-values #+:WIN32 26 #-:WIN32 25 15)))
                         :font font
                         :x-alignment :center
                         :y-alignment :bottom)
        (when (and axis-values
                   x-axis-values)
          (let* ((_xmin (plotter-xmin cpw))
                 (_xmax (plotter-xmax cpw))
                 (_xlast nil)
                 (_xstart nil))
            (destructuring-bind (x0 dx nl nu) (calc-start-delta _xmin _xmax)
              (declare (ignore nl nu))
              (if xlog
                  (setf dx 1))
              (labels ((xwork (xval xprev)
                         (let* ((xpos  (gp:transform-point (plotter-xform cpw)
                                                           xval 0))
                                (xlast (draw-string-x-y
                                        cpw port (cond ((functionp x-values)
                                                        (funcall x-values xval))
                                                       ((consp x-values)
                                                        (elt x-values (round xval)))
                                                       (t
                                                        (plabel (iqxlog xval)))
                                                       )
                                        (* sf xpos)
                                        (* sf (+ #+:WIN32 2 #-:WIN32 3 (box-bottom box)))
                                        :prev-bounds xprev
                                        :margin (* 2 sf)
                                        :x-alignment :center
                                        :y-alignment :top
                                        :font font)))
                           
                           (gp:with-graphics-scale (port sf sf)
                             (gp:with-graphics-state (port
                                                      :scale-thickness t)
                               (when fullgrid
                                 (when xlog
                                   (with-color (port #.(color:make-gray 0.75))
                                     (let ((xscale (first (plotter-xform cpw))))
                                       (loop for ix in *log-subdivs* do
                                             (let ((x (+ xpos (* xscale ix))))
                                               (if (< (box-left box) x
                                                      (box-right box))
                                                   (gp:draw-line
                                                    port
                                                    x (box-top box)
                                                    x (box-bottom box))
                                                 )))
                                       )))
                                 (unless (zerop xval)
                                   (with-color (port (if (vectorp fullgrid)
                                                         fullgrid
                                                       (color:make-gray
                                                        (if xlog 0.5 0.75))))
                                     (gp:draw-line port
                                                   xpos (box-top box)
                                                   xpos (box-bottom box))
                                     )))
                               
                               (gp:draw-line port
                                             xpos (- (box-bottom box) 2)
                                             xpos (+ (box-bottom box) 3))))
                           
                           xlast)))
                
                
                (loop for xval = x0 then (- xval dx)
                      until (< xval (if (> _xmax _xmin) _xmin _xmax))
                      do
                      (progn ;; ignore-errors
                        (setf _xlast (xwork xval _xlast)))
                      (unless _xstart
                        (setf _xstart _xlast)))
                
                (setf _xlast _xstart)
                
                (loop for xval = (+ x0 dx) then (+ xval dx)
                      until (> xval (if (< _xmin _xmax) _xmax _xmin))
                      do
                      (progn ;; ignore-errors
                        (setf _xlast (xwork xval _xlast))))
                )))))
        
      (when ytitle
        (draw-vert-string-x-y cpw port ytitle
                              #+(AND :WIN32 (NOT (OR :LISPWORKS6.1 :LISPWORKS7))) 0
                              #+(AND :COCOA (NOT (OR :LISPWORKS6.1 :LISPWORKS7))) (* sf (if axis-values 3 15))
                              #+(OR :LISPWORKS6.1 :LISPWORKS7)                    (* sf (if axis-values 15 20))
                              (floor (* sf (+ (box-top box)
                                              (box-bottom box))) 2)
                              :font  font
                              :x-alignment :center
                              :y-alignment :top)
        (when (and axis-values
                   y-axis-values)
          (let* ((_ymin (plotter-ymin cpw))
                 (_ymax (plotter-ymax cpw))
                 (_ylast  nil)
                 (_ystart nil))
            (destructuring-bind (y0 dy nl nu) (calc-start-delta _ymin _ymax)
              (declare (ignore nl nu))
              (if ylog
                  (setf dy 1))
              (labels ((ywork (yval yprev)
                         (multiple-value-bind (xpos ypos)
                             (gp:transform-point (plotter-xform cpw) 0 yval)
                           (declare (ignore xpos))
                           (let ((ylast (draw-vert-string-x-y
                                         cpw port
                                         (plabel (iqylog yval))
                                         (* sf (- (box-left box) #+:WIN32 1 #-:WIN32 3))
                                         (* sf ypos)
                                         :prev-bounds yprev
                                         :margin (* 2 sf)
                                         :x-alignment :center
                                         :y-alignment :bottom
                                         :font font)))
                             
                             (gp:with-graphics-scale (port sf sf)
                               (gp:with-graphics-state (port :scale-thickness t)
                                 (when fullgrid
                                   (when ylog
                                     (with-color (port #.(color:make-gray 0.75))
                                       (let ((yscale (fourth (plotter-xform cpw))))
                                         (loop for ix in *log-subdivs* do
                                               (let ((y (+ ypos (* yscale ix))))
                                                 (if (> (box-bottom box) y
                                                        (box-top box))
                                                     (gp:draw-line
                                                      port
                                                      (1+ (box-left box)) y
                                                      (box-right box) y)
                                                   ))))
                                       ))
                                   (unless (zerop yval)
                                     (with-color (port (if (vectorp fullgrid)
                                                           fullgrid
                                                         (color:make-gray
                                                          (if ylog 0.5 0.75))))
                                       (gp:draw-line port
                                                     (1+ (box-left box))  ypos
                                                     (box-right box) ypos)
                                       )))
                                 
                                 (gp:draw-line port
                                               (- (box-left box) 2) ypos
                                               (+ (box-left box) 3) ypos)))
                             ylast))))
                
                (loop for yval = y0 then (- yval dy)
                      until (< yval (if (> _ymax _ymin) _ymin _ymax))
                      do
                      (progn ;; ignore-errors
                        ;; even though we are running without-denormal processing
                        ;; GP seems to produce denorms and then balks at the result
                        ;; we have to protect ourselves here... (LWM-64 fails, but LWM-32 runs okay)
                        (setf _ylast (ywork yval _ylast)))
                      (unless _ystart
                        (setf _ystart _ylast)))
                
                (setf _ylast _ystart)
                
                (loop for yval = (+ y0 dy) then (+ yval dy)
                      until (> yval (if (< _ymin _ymax) _ymax _ymin))
                      do
                      (progn ;; ignore-errors
                        (setf _ylast (ywork yval _ylast))))
                ))))
        ))
    ))

