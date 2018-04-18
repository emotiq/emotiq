
(in-package :plotter)

;; ---------------------------------------------------------
;; org can be a list of (type xorg yorg), e.g., '(:frac 0.9 0.96)
;; or a pair of typed values ((type xorg) (type yorg)), e.g., '((:frac 0.9) (:data 14.3))
;;
;; convert to a list of typed pairs, e.g., '((:frac 0.9) (:data 14.3))
;;
(defun get-xy-orgs (org)
  (if (= 3 (length org))
      (list (list (first org) (second org))
            (list (first org) (third org)))
    org))

(defun draw-text (pane str org &rest args)
  (destructuring-bind (xorg yorg) (get-xy-orgs org)
    (apply #'outsxy pane xorg yorg str (append args *default-args*))))

;; ------------------------------------------
(defun do-plot-histogram (pane v &rest args
                            &key min max range nbins binwidth
                            ylog cum (norm t)
                            (line-type :stepped)
                            &allow-other-keys)
  (multiple-value-bind (x h bw)
      (vm:histogram v
                    :min      min
                    :max      max
                    :range    range
                    :nbins    nbins
                    :binwidth binwidth)
    (let* ((nel (array-total-size v))
           (tot (* nel bw))
           minnz)
      (when norm
        (loop for v across h
              for ix from 0
              do
              (setf (aref h ix) (/ v tot))
              ))
      (when cum
        (loop for vy across h
              for ix from 0
              for sf = (if norm bw (/ nel))
              for sum = (* sf vy) then (+ sum (* sf vy))
              do
              (setf (aref h ix) sum)
              (unless (or minnz
                          (zerop sum))
                (setf minnz sum))
              ))
      (when ylog
        (let ((zlim (cond (cum  minnz)
                          (norm (/ 0.9 tot))
                          (t     0.9)
                          )))
          (loop for v across h
                for ix from 0
                do
                (when (zerop v)
                  (setf (aref h ix) zlim)))
          ))
      (apply #'plot pane x h :line-type line-type args)
      )))

;; user callable routine
(defun histogram (pane v &rest args)
  (apply #'do-plot-histogram pane (coerce v 'vector) (append args *default-args*)))

