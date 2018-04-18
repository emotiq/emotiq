
(in-package :plotter)

;; ------------------------------------------

(defun log10 (x)
  (if (not (plusp x))
      -300
    (log x 10.0d0)))

(defun pow10 (x)
  (expt 10.0d0 x))

;; ------------------------------------------
(defun qrange (rng &optional (default 0.1))
  (if (zerop rng)
      default
    rng))

(defun qdiv (a b &optional (default 0.1))
  (/ a (qrange b default)))

;; ------------------------------------------
(defun get-range (range v islog)
  (if (and range
           (/= (first range) (second range)))
      range
    (let ((v (if islog
                 (remove-if (complement #'plusp) v)
               v)))
      (if (plusp (length-of v))
          (let* ((vmin (vmin-of v))
                 (vmax (vmax-of v)))
            (if (= vmin vmax)
                (setf vmax (if (zerop vmin)
                               0.1
                             (* 1.1 vmin))))
            (list vmin vmax))
        (list (if islog 0.1 0) 1))
      )))

(defconstant $largest-permissible-value
  (/ least-positive-normalized-double-float))

(defmethod pw-init-xv-yv ((cpw <plotter-mixin>) xv yv
                          &key xrange yrange box xlog ylog aspect
                          &allow-other-keys)
  ;; initialize basic plotting parameters -- log scale axes, axis ranges,
  ;; plotting interior region (the box), and the graphic transforms to/from
  ;; data space to "pixel" space.  Pixel in quotes because they are real pixels
  ;; on Win/XP, but something altogether different on OS/X Display PDF.
  (let* ((_box (or box
                   (inset-box-sides (list 0 0
                                          (plotter-nominal-width  cpw)
                                          (plotter-nominal-height cpw))
                                    30 20 10 30)
                   )))
    (destructuring-bind (_xmin _xmax)
        (if xv
            (get-range xrange xv xlog)
          (get-range xrange (list 0 (1- (length-of yv))) xlog))
      (destructuring-bind (_ymin _ymax) (get-range yrange yv ylog)

        (if xlog
            (setf _xmin (log10 _xmin)
                  _xmax (log10 _xmax)))
        (if ylog
            (setf _ymin (log10 _ymin)
                  _ymax (log10 _ymax)))
        
        (unless yrange
          (let ((dy (/ (qrange (- _ymax _ymin)) 18)))
            (setf _ymin (max (- _ymin dy) (- $largest-permissible-value)))
            (setf _ymax (min (+ _ymax dy) $largest-permissible-value))
            ))
        
        (unless xrange
          (let ((dx (/ (qrange (- _xmax _xmin)) 18)))
            (setf _xmin (max (- _xmin dx) (- $largest-permissible-value)))
            (setf _xmax (min (+ _xmax dx) $largest-permissible-value))
            ))
        
        (setf (plotter-box  cpw) _box
              (plotter-xmin cpw) _xmin
              (plotter-xmax cpw) _xmax
              (plotter-ymin cpw) _ymin
              (plotter-ymax cpw) _ymax
              (plotter-xlog cpw) xlog
              (plotter-ylog cpw) ylog)
        
        (let ((xscale (qdiv (- (box-right _box) (box-left _box))
                            (- _xmax _xmin)))
              (yscale (qdiv (- (box-bottom _box) (box-top _box))
                            (- _ymax _ymin))))
          
          (if (and (numberp aspect)
                   (plusp aspect))
              
              (let* ((x-squeeze (<= aspect 1))
                     (scale     (if x-squeeze
                                    (max xscale yscale)
                                  (min xscale yscale))))
                (setf xscale (if x-squeeze
                                 (* aspect scale)
                               scale)
                      yscale (if x-squeeze
                                 scale
                               (/ scale aspect))
                      (box-right  _box) (+ (box-left _box) (* (- _xmax _xmin) xscale))
                      (box-bottom _box) (+ (box-top  _box) (* (- _ymax _ymin) yscale)))
                ))
          
          (let ((xform     (gp:make-transform))
                (inv-xform (gp:make-transform)))
            (gp:apply-translation xform (- _xmin) (- _ymin))
            (gp:apply-scale xform xscale (- yscale))
            (gp:apply-translation xform (box-left _box) (box-bottom _box))
            (gp:invert-transform xform inv-xform)
            (setf (plotter-xform     cpw) xform
                  (plotter-inv-xform cpw) inv-xform)
            )))
      )))

;; ---------------------------------------------------------

(defun vector-group-min (yvecs)
  (reduce #'min (mapcar #'vmin-of yvecs)))

(defun vector-group-max (yvecs)
  (reduce #'max (mapcar #'vmax-of yvecs)))

(defun pw-init-bars-xv-yv (cpw xvec yvecs &rest args)
  ;; just run the usual scaling initialization
  ;; but against a y-vector that contains those values
  ;; from the multiple vectors which have the largest absolute values
  (apply #'pw-init-xv-yv cpw
         (or (and xvec
                  (list (vmin-of xvec) (vmax-of xvec)))
             (and yvecs
                  (list 0 (1- (length-of (first yvecs))))
                  ))
         (and yvecs
              (list (vector-group-min yvecs)
                    (vector-group-max yvecs)))
         args))

