
(in-package :plotter)

;; ------------------------------------------
;; Convenience macros

(defmacro with-color ((pane color) &body body)
  `(gp:with-graphics-state
       (,pane
        :foreground ,color)
     ,@body))
  
(defmacro with-mask ((pane mask) &body body)
  `(gp:with-graphics-state
       (,pane
        :mask ,mask)
     ,@body))

