
(in-package :plotter)

;; ----------------------------------------------------------------

(defun draw-existing-legend (pane port)
  (let* ((legend (plotter-legend pane))
         (items  (has-content legend)))
    (when items
      (let* ((sf   (plotter-sf pane))
             (font1 (find-best-font port
                                    :size $tiny-times-font-size))
             (font (find-best-font port
                                   :size (* sf $tiny-times-font-size)
                                   ;; :size $tiny-times-font-size
                                   ))
             (nitems (length items)))
        
        (multiple-value-bind (txtwd txtht txtbase)
            (let ((maxwd   0)
                  (maxht   0)
                  (maxbase 0))
              (dolist (item items)
                (multiple-value-bind (lf tp rt bt)
                    (gp:get-string-extent port (legend item) font)
                  (setf maxwd   (max maxwd (- rt lf))
                        maxht   (max maxht (- bt tp))
                        maxbase (max maxbase tp))))
              (values maxwd maxht maxbase))
          
          (declare (ignore txtbase))
          (let* ((totwd  (+ txtwd  (* sf 40)))
                 (totht  (+ 2 (* nitems (+ txtht 0))))
                 (effwd  (/ totwd sf))
                 (effht  (/ totht sf))
                 (effht1 (/ txtht sf))
                 (x      (or (preferred-x pane)
                             (let ((x (get-x-location pane (plotter-legend-x pane))))
                               (ecase (plotter-legend-anchor pane)
                                 (:auto         (if (> x (/ (plotter-nominal-width pane) 2))
                                                    (- x effwd)
                                                  x))
                                 ((:nw :w :sw)  (- x effwd))
                                 ((:ne :e :se)  x)
                                 ((:n  :ctr :s) (- x (/ effwd 2)))
                                 ))))
                 (y      (or (preferred-y pane)
                             (let ((y (get-y-location pane (plotter-legend-y pane))))
                               (case (plotter-legend-anchor pane)
                                 (:auto         (if (> y (/ (plotter-nominal-height pane) 2))
                                                    (- y effht)
                                                  y))
                                 ((:nw :n :ne)  y)
                                 ((:sw :s :sw)  (- y effht))
                                 ((:w  :ctr :e) (- y (/ effht 2)))
                                 )))))
            
            (setf (x legend)      (* sf x)
                  (y legend)      (* sf y)
                  (width legend)  totwd
                  (height legend) totht)
            
            (gp:with-graphics-scale (port sf sf)
              
              (with-color (port (adjust-color port (background-color port) 0.75))
                (gp:draw-rectangle port x y effwd effht
                                   :filled t))
              
              (gp:with-graphics-state (port
                                       :thickness (adjust-linewidth sf))
                (gp:draw-rectangle port x y effwd effht))
              
              (loop for item in items
                    for y from (+ y effht1 1) by effht1
                    do
                    (let* ((line-style   (line-style   item))
                           (symbol-style (symbol-style item)))
                      
                      ;; ---------------------------------------------
                      (labels ((draw-line (&optional thickness)
                                 (gp:with-graphics-state
                                     (port
                                      :thickness  (adjust-linewidth
                                                   (* #| sf |# (or thickness
                                                             (line-thick line-style))))
                                      :dashed     (and line-style
                                                       (line-dashing line-style))
                                      :dash       (mapcar (um:expanded-curry (v) #'* sf)
                                                          (and line-style
                                                               (line-dashing line-style)))
                                      :foreground (if line-style
                                                      (adjust-color port
                                                                    (line-color line-style)
                                                                    (line-alpha line-style))
                                                    (adjust-color port
                                                                  (fill-color symbol-style)
                                                                  (fill-alpha symbol-style))))
                                   (let ((y (floor (- y (/ effht1 2)))))
                                     (gp:draw-line port
                                                   (+ x  3) y
                                                   (+ x 33) y)
                                     ))))
                        
                        ;; ---------------------------------------------
                        (cond  (symbol-style
                                (case (plot-symbol symbol-style)
                                  ((:vbars :hbars) (draw-line 5))
                                  (otherwise
                                   (when line-style
                                     (draw-line))
                                   (funcall (get-symbol-plotfn port sf symbol-style)
                                            (+ x 18) (- y (/ effht1 2))
                                            ))
                                  ))
                               
                               (line-style (draw-line))
                               ))
                      
                      ;; ---------------------------------------------
                      (gp:draw-string port (legend item) (+ x 36) (- y 3)
                                      :font font1)
                      
                      ))
              ))
          )))
    ))

(defun draw-accumulated-legend (pane port)
  (let ((items  (all-legends pane))
        (legend (plotter-legend pane)))

    (cond ((null items)
           (setf (has-content legend) nil))
    
          ((activep legend)
           (setf (has-content legend) items)
           (draw-existing-legend pane port))
          )))

(defun highlight-legend (pane)
  (let ((legend (plotter-legend pane)))
    (when (activep legend)
      (unless (highlighted legend)
        (setf (highlighted legend) t)
        (let ((sf (plotter-sf pane)))
          (gp:draw-rectangle pane (x legend) (y legend)
                             (width legend) (height legend)
                             :thickness (* 2 sf))
          )))))

(defun restore-legend-background (pane legend)
  (let* ((sf      (plotter-sf pane))
         (extra   (* 2 sf))
         (extra*2 (* 2 extra))
         (w       (+ extra*2 (width legend)))
         (h       (+ extra*2 (height legend)))
         (x-old   (- (x legend) extra))
         (y-old   (- (y legend) extra)))
    (gp:copy-pixels pane (plotter-backing-pixmap pane)
                    x-old y-old w h x-old y-old)
    ))

(defun unhighlight-legend (pane)
  (let ((legend (plotter-legend pane)))
    (when (and (activep legend)
               (highlighted legend))
      (setf (highlighted legend) nil)
      (restore-legend-background pane legend)
      (draw-existing-legend pane pane)
      )))

(defun on-legend (pane x y)
  (let ((legend (plotter-legend pane)))
    (and (activep legend)
         (has-content legend)
         (<= (x legend) x (+ (x legend) (width legend)))
         (<= (y legend) y (+ (y legend) (height legend)))
         )))

(defun start-drag-legend (pane x y)
  (let ((legend (plotter-legend pane)))
    (when (and (activep legend)
               (has-content legend))
      (setf (dragging legend) t
            (dx legend) (- (x legend) x)
            (dy legend) (- (y legend) y))
      )))

(defun undrag-legend (pane x y)
  (declare (ignore x y))
  (let ((legend (plotter-legend pane)))
    (when (dragging legend)
      (setf (dragging legend) nil)
      )))

(defun drag-legend (pane x y)
  (let ((legend (plotter-legend pane)))
    (when (dragging legend)
      (restore-legend-background pane legend)
      (let ((sf (plotter-sf pane)))
        (setf (preferred-x pane) (/ (+ x (dx legend)) sf)
              (preferred-y pane) (/ (+ y (dy legend)) sf))
        (draw-existing-legend pane pane)
        ))))


#|
(setf plt (plt:wset 'plt))
(plt:fplot plt '(-10 10) (lambda (x) (/ (sin x) x))
           :thick 2
           :legend "Sinc(x)"
           :clear t)

|#
