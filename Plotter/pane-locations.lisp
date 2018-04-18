
(in-package :plotter)

;; ----------------------------------------------------------------
;; pane location decoding for strings and legends
;;
(defun parse-location (sym pane)
  ;; BEWARE:: '1.2d+3p means (:pixel 1200) NOT (:data 1.2 +3)
  ;; be sure to disambiguate the "d" as in '1.2data+3p
  (let* ((s    (if (stringp sym)
                   sym
                 (symbol-name sym)))
         (slen (length s)))
    
    (labels ((iter (state ix ans)
               (ecase state
                 (:initial
                  (cond ((>= ix slen)
                         ;; have been all number constituents
                         ;;assume we have pixels specified
                         (list :pixel (read-from-string s) 0))
                        
                        ((digit-char-p (char s ix))
                         ;; still have number constituent
                         (iter :initial (1+ ix) nil))
                        
                        ((char= #\. (char s ix))
                         ;; still have number constituent
                         (iter :initial (1+ ix) nil))
                        
                        ((char-equal #\d (char s ix))
                         ;; might be part of an exponential notation
                         ;; or it might be a frac or data specifier
                         (iter :check-for-exponent (1+ ix) nil))
                        
                        ((char-equal #\t (char s ix))
                         ;; 't' for top
                         (iter :scan-to-plus-or-minus (1+ ix)
                               (list :pixel (plotter-nominal-height pane))
                               ))
                        
                        ((char-equal #\r (char s ix))
                         ;; 'r' for right
                         (iter :scan-to-plus-or-minus (1+ ix)
                               (list :pixel (plotter-nominal-width pane))
                               ))

                        ((or (char-equal #\b (char s ix))
                             (char-equal #\l (char s ix)))
                         ;; 'b' for bottom, 'l' for left
                         (iter :scan-to-plus-or-minus (1+ ix)
                               (list :pixel 0)
                               ))
                        
                        (t
                         (iter :get-units ix (read-from-string s t :eof :end ix)))
                        ))
                 
                 (:check-for-exponent
                  (cond ((>= ix slen)
                         ;; we had a D so it must have been (:data nn.nnn)
                         (list :data
                               (read-from-string s t :eof :end (1- ix))
                               ))
                        
                        ((alpha-char-p (char s ix))
                         ;; can't be a +/- sign, so must have been (:data nn.nn)
                         (iter :scan-to-plus-or-minus (1+ ix)
                               (list :data
                                     (read-from-string s t :eof :end (1- ix))
                                     )))
                        
                        (t ;; assume we have a +/- sign and continue with number scan
                           (iter :initial (1+ ix) nil))
                        ))
                  
                 (:get-units
                  (ecase (char-upcase (char s ix))
                    (#\P  (iter :scan-to-plus-or-minus (1+ ix) (list :pixel ans)))
                    (#\D  (iter :scan-to-plus-or-minus (1+ ix) (list :data  ans)))
                    (#\F  (iter :scan-to-plus-or-minus (1+ ix) (list :frac  ans)))))
                 
                 (:scan-to-plus-or-minus
                  (cond ((>= ix slen)
                         ;; no offset, we a finished
                         ans)
                        
                        ((or (char= #\+ (char s ix))
                             (char= #\- (char s ix)))
                         (let ((end (position-if #'alpha-char-p s :start ix)))
                           ;; read the offset and return
                           (list (first ans)  ;; type
                                 (second ans) ;; value
                                 (read-from-string s t :eof :start ix :end end) ;; offset
                                 )))
                        
                        (t ;; keep looking
                           (iter :scan-to-plus-or-minus (1+ ix) ans))
                        ))
                 )))
      
      (iter :initial 0 nil)
      )))

(defun get-location (pane pos-expr axis &key scale)
  (cond
   
   ((consp pos-expr)
    (let* ((sym (um:mkstr (first pos-expr)))
           (val (second pos-expr)))
      (ecase (char-upcase (char sym 0))
        ;; accommodates :DATA :DAT :D :DATUM, :FRAC :F :FRACTION, :PIXEL :P :PIX :PIXELS, etc.
        (#\F  ;; pane fraction  0 = left, bottom;  1 = right, top
              (ecase axis
                (:x  (* val (plotter-nominal-width pane)))

                ;; port y axis is inverted, top at 0
                (:y  (* (- 1 val) (plotter-nominal-height pane)))
                ))
        
        (#\D  ;; data coordinates
              (ecase axis
                (:x
                 (let ((ans (gp:transform-point (plotter-xform pane)
                                                (if (plotter-xlog pane)
                                                    (log10 val)
                                                  val)
                                                0)))
                   (if scale
                       (- ans (gp:transform-point (plotter-xform pane) 0 0))
                     ans)))
                                                  
                (:y
                 (let ((ans (multiple-value-bind (xx yy)
                                (gp:transform-point (plotter-xform pane)
                                                    0
                                                    (if (plotter-ylog pane)
                                                        (log10 val)
                                                      val))
                              (declare (ignore xx))
                              yy)))
                   (if scale
                       (- ans (second (multiple-value-list
                                       (gp:transform-point (plotter-xform pane)
                                                           0 0))))
                     ans)))
                ))
        
        (#\P  ;; direct pixel positioning
              (ecase axis
                (:x val)

                ;; port y axis is inverted, top at 0
                (:y (- (plotter-nominal-height pane) val 1))
                ))
        )))

   ((numberp pos-expr) ;; assume :DATA
    (get-location pane (list :data pos-expr) axis :scale scale))

   (t ;; else, expect a parsable symbol or string '1.2data+3pix
      (destructuring-bind (vtype v &optional (dv 0)) (parse-location pos-expr pane)
        (+ (get-location pane (list vtype v) axis :scale scale)
           (ecase axis
             (:x dv)
             (:y (- dv))  ;; port y axis is inverted, top at 0
             ))))
   ))
  
(defun get-x-location (pane x)
  (get-location pane x :x))

(defun get-y-location (pane y)
  (get-location pane y :y))

(defun get-x-width (pane wd)
  (get-location pane wd :x :scale t))

(defun get-y-width (pane wd)
  (get-location pane wd :y :scale t))

(defun get-x-for-location (pane x)
  (let ((ans (gp:transform-point (plotter-inv-xform pane) x 0)))
    (if (plotter-xlog pane)
        (pow10 ans)
      ans)))

(defun get-y-for-location (pane y)
  (multiple-value-bind (xv yv) (gp:transform-point (plotter-inv-xform pane) 0 y)
    (declare (ignore xv))
    (if (plotter-ylog pane)
        (pow10 yv)
      yv)))
