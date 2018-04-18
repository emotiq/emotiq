
(in-package "PLOTTER")

(defun sinc (x)
  (let ((v (* pi x)))
    (/ (sin v) v)))

(fplot 'plt '(-10 10) 'sinc :clear t :thick 2 :legend "Sinc")

(fplot 'plt '(-10 10) 'sinc
       :clear t
       :plot-style
       `(:line-style (:thick 3
                      :color :red3
                      :alpha 0.3)
         :symbol-style (:symbol :circle
                        :border-thick 1
                        :border-color :black
                        :border-alpha 0.2
                        :fill-color :green3
                        :fill-alpha   0.2))
       :legend "Sinc")

(fplot 'plt '(-10 10) 'sinc
       :clear t
       :plot-style
       `(:symbol-style (:symbol :circle
                        :border-thick 1
                        :border-alpha 0.2
                        :border-color :black
                        :fill-color :green3
                        :fill-alpha 0.2))
       :legend "Sinc")

(fplot 'plt '(-10 10) 'sinc
       :clear t
       :line-style '(:type :stepped))

(let ((ys (loop for ix from 1 below 50 collect (sinc (/ ix 10)))))
  (plot 'plt ys
        :clear t
        ;;:line-style   '(:color :red3 :alpha 0.3)
        :symbol-style '(:symbol :sampled-data
                        :fill-color :darkgreen
                        :fill-alpha 0.3
                        :border-color :green3
                        :border-thick 1)))