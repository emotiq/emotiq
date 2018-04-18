
(in-package :plotter)

;; ---------------------------------------------
;; Define some safe image access macros...
;;
(defmacro with-image ((port (image imgexpr)) &body body)
  ;; returned value will be that of the body
  `(let ((,image ,imgexpr))
     (unwind-protect
         (progn
           ,@body)
       (gp:free-image ,port ,image))
     ))

(defmacro with-image-access ((acc access-expr) &body body)
  ;; returned value will be that of the body
  `(let ((,acc ,access-expr))
     (unwind-protect
         (progn
           ,@body)
       (gp:free-image-access ,acc))
     ))

;; ------------------------------------------
;; We can use WITH-DELAYED-UPDATE to ward off immediate and slowing
;; direct drawing operations. Delayed sections can be nested. Meanwhile,
;; within a delayed section we are simply building up a display list of
;; parameterized lambda closures that collectively will produce the sum
;; of all delayed operations, once the delay goes back to zero.
;;
(defun do-sync-with-capi (capi-fn capi-elt fn args)
  (funcall capi-fn capi-elt
           #'apply fn args))

(defmethod sync-with-capi ((intf capi:interface) fn &rest args)
  (do-sync-with-capi #'capi:execute-with-interface intf fn args))

(defmethod sync-with-capi ((pane capi:simple-pane) fn &rest args)
  (do-sync-with-capi #'capi:apply-in-pane-process pane fn args))

(defmethod sync-with-capi ((pane capi:pinboard-object) fn &rest args)
  (let ((layout (capi:pinboard-object-pinboard pane)))
    (when layout
      (apply 'sync-with-capi layout fn args))
    ))

(defmethod sync-with-capi (pane fn &rest args)
  (declare (ignore pane fn args)))

;; ------------------------------------------
#|
(defun do-with-locked-plotter-pane (pane fn)
  (mpcompat:with-lock ((plotter-lock (plotter-mixin-of pane)))
    (funcall fn)))

(defmacro with-locked-plotter-pane (pane &body body)
  `(do-with-locked-plotter-pane ,pane (lambda () ,@body)))
|#

;; ------------------------------------------

(defmethod redraw-entire-pane ((pane <plotter-pane>))
  (capi:apply-in-pane-process pane
                              (lambda ()
                                (setf (plotter-dirty pane) t)
                                #-:WIN32
                                (gp:invalidate-rectangle pane)
                                #+:WIN32
                                (win32-display-callback pane 0 0
                                                        (gp:port-width pane)
                                                        (gp:port-height pane))
                                )))

#|
(defmethod redraw-entire-pane ((pane capi:pinboard-object))
  (progn ;; with-locked-plotter-pane pane
    (setf (plotter-dirty pane) t)
    #-:WIN32
    (gp:invalidate-rectangle pane)
    ))
|#

(defun do-with-delayed-update (pane fn)
  (let ((pane (plotter-mixin-of pane))) ;; could be called with symbolic name for pane
    (let* ((prev-ct (plotter-delayed-update pane))
           (changed (when (zerop prev-ct)
                      ;; this also clears the changed indication
                      (um:nchanged-p (plotter-display-list pane)))))
      (incf (plotter-delayed-update pane))
      (unwind-protect
          (prog1
              (funcall fn)
            (when (and (zerop prev-ct)
                       (or changed
                           (um:nchanged-p (plotter-display-list pane)))
                       (setf (plotter-dirty pane) t)
                       (sync-with-capi pane 'redraw-entire-pane pane)))
            (decf (plotter-delayed-update pane))
            ))
      )))
#|
;; test delayed updates -- entire composite plot should appear at one time
(let ((win (plt:wset 'myplot)))
  (plt:with-delayed-update (win)
    ;; (plt:clear win)
    (plt:fplot win '(-20 20) (lambda (x) (/ (sin x) x))
               :thick 2
               :title "Sinc"
               :clear t)
    (sleep 2)
    (plt:fplot win '(-20 20) (lambda (x) (/ (sin x) x))
               :symbol :circle
               :color :blue)))
|#
#|
(defun do-with-delayed-update (pane fn)
  (let* ((pane (plotter-mixin-of pane)) ;; could be called by user with symbolic name for pane
         (ct   (plotter-delayed-update pane)))
    (incf (plotter-delayed-update pane))
    (when (zerop ct) ;; asking if changed resets the changed indiction
      (um:changed-p (plotter-display-list pane)))
    (unwind-protect
        (progn
          (funcall fn)
          (when (and (zerop ct)
                     (um:changed-p (plotter-display-list pane)))
            (sync-with-capi pane
                            (lambda ()
                              (discard-backing-pixmap pane)
                              (gp:invalidate-rectangle pane))
                            )))
      (decf (plotter-delayed-update pane))
      )))
|#
#|
;; capi:with-atomic-redisplay does not nest properly
(defun do-with-delayed-update (pane fn)
  (capi:with-atomic-redisplay (pane)
    (funcall fn)
    (discard-backing-pixmap pane)
    (gp:invalidate-rectangle pane)))
|#

;; user callable macro
(defmacro with-delayed-update ((pane) &body body)
  `(do-with-delayed-update ,pane
    (lambda ()
      ,@body)))


(defun do-wait-until-finished (pane mbox timeout fn)
  (if (eq mp:*current-process* mp:*main-process*)
      (with-delayed-update (pane)
        (funcall fn))
    (let ((mbox (or mbox (mp:make-mailbox))))
      (with-delayed-update (pane)
        (setf (reply-mbox (plotter-mixin-of pane)) mbox)
        (funcall fn))
      (mp:mailbox-read mbox "Waiting for plotter to finish" timeout)
      )))
    
(defmacro wait-until-finished ((pane &key mbox timeout) &body body)
  `(do-wait-until-finished ,pane ,mbox ,timeout (lambda () ,@body)))



