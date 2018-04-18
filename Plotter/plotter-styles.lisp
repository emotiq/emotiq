
(in-package :plotter)

;; ----------------------------------------------------------------

(defun get-plot-style (&key
                       (color #.(color:make-rgb 0.0 0.5 0.0))
                       (line-color color)
                       alpha
                       (line-alpha alpha)
                       thick
                       (linewidth (or thick 1))
                       (line-thick linewidth)
                       line-dashing
                       (line-type :interpolated)
                       symbol
                       plot-joined
                       (border-color color)
                       (border-alpha alpha)
                       symbol-filled
                       (fill-color color)
                       (fill-alpha alpha)
                       (border-thick linewidth)
                       bar-width
                       bar-offset
                       legend
                       plot-style
                       line-style
                       symbol-style
                       &allow-other-keys)
  (cond ((consp plot-style)
         (let ((line-style   (getf plot-style :line-style))
               (symbol-style (getf plot-style :symbol-style))
               (legend       (or legend
                                 (getf plot-style :legend))))
           (make-instance '<plot-style>
                          :line-style   (and line-style
                                             (apply 'make-instance '<line-style> line-style))
                          :symbol-style (and symbol-style
                                             (apply 'make-instance '<symbol-style> symbol-style))
                          :legend       (and legend
                                             (plusp (length legend))
                                             legend)
                          )))
        
        (plot-style)
        
        (t
         (let ((sym (or symbol
                        (and symbol-style
                             (or (and (consp symbol-style)
                                      (getf symbol-style :symbol))
                                 (plot-symbol symbol-style))))))
           
           (make-instance '<plot-style>
                          :line-style (cond ((consp line-style)
                                             (apply 'make-instance '<line-style> line-style))
                                            
                                            (line-style)
                                            
                                            ((or (null sym)
                                                 (eq sym :sampled-data)
                                                 plot-joined)
                                             (make-instance '<line-style>
                                                            :thick   line-thick
                                                            :dashing line-dashing
                                                            :color   line-color
                                                            :alpha   line-alpha
                                                            :type    line-type)))
                          
                          :symbol-style (cond ((consp symbol-style)
                                               (apply 'make-instance '<symbol-style> symbol-style))
                                              
                                              (symbol-style)
                                              
                                              (symbol
                                               (make-instance '<symbol-style>
                                                              :symbol  (case symbol
                                                                         ((:filled-circle :sampled-data)         :circle)
                                                                         ((:filled-square :filled-box)           :square)
                                                                         ((:filled-triangle :filled-up-triangle) :up-triangle)
                                                                         (:filled-down-triangle                  :down-triangle)
                                                                         (otherwise symbol))
                                                              :fill-color   (or fill-color
                                                                                (and (or symbol-filled
                                                                                         (member symbol
                                                                                                 '(:filled-circle
                                                                                                   :sampled-data
                                                                                                   :filled-square
                                                                                                   :filled-box
                                                                                                   :filled-triangle
                                                                                                   :filled-up-triangle
                                                                                                   :filled-down-triangle)))
                                                                                     color))
                                                              :fill-alpha   fill-alpha
                                                              :border-color border-color
                                                              :border-alpha border-alpha
                                                              :border-thick border-thick
                                                              :bar-width    bar-width
                                                              :bar-offset   bar-offset)))

                          :legend (and legend
                                       (plusp (length legend))
                                       legend)
                          )))
        ))

