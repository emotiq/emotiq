
(in-package :plotter)

;; ------------------------------------------
(defmethod plotter-mixin-of (name &optional args)
  ;; allow for symbolic names in place of plotter-windows or
  ;; <plotter-pane>s. Names must match under EQUALP (i.e., case
  ;; insensitive strings, symbols, numbers, etc.)
  (apply 'wset name (append args *default-args*)))

;; -------------------------------------
(defun locate-plotter-window (name)
  (find name (capi:collect-interfaces '<plotter-window>)
        :test #'equalp
        :key  #'capi:capi-object-name))
;; --------------------------------------

(defun find-named-plotter-pane (name)
  ;; locate the named plotter window and return its <plotter-pane> object
  (let ((win (locate-plotter-window name)))
    (and win
         (plotter-mixin-of win))))

;; ---------------------------------------------------------------
(defclass <plotter-window> (capi:interface)
  ((drawing-area  :accessor drawing-area  :initarg :drawing-area)))

(defmethod plotter-mixin-of ((intf <plotter-window>) &optional args)
  (declare (ignore args))
  (drawing-area intf))

(defmethod display-cursor-readout ((intf <plotter-window>) name x y)
  (setf (capi:interface-title intf)
        (format nil "~A  x = ~,6g  y = ~,6g"
                name x y)))
  
(defun make-plotter-window (&key
                            (name       0)
                            (title      "Plot")
                            (fg         :black)
                            (bg         :white)
                            (foreground fg)
                            (background bg)
                            (xsize      400)
                            (ysize      300)
                            xpos
                            ypos
                            (best-width         xsize)
                            (best-height        ysize)
                            (best-x             xpos)
                            (best-y             ypos)
                            (visible-min-width  (/ xsize 2))
                            (visible-min-height (/ ysize 2))
                            (visible-max-width  (* xsize 2))
                            (visible-max-height (* ysize 2))
                            cursor
                            full-crosshair
                            window-styles
                            window-class)
  
  (let ((pane (make-instance '<plotter-pane>
                             :name               name
                             :background         background
                             :foreground         foreground
                             :nominal-width      best-width
                             :nominal-height     best-height
                             :visible-min-width  visible-min-width
                             :visible-max-width  visible-max-width
                             :visible-min-height visible-min-height
                             :visible-max-height visible-max-height
                             :cursor             cursor
                             :full-crosshair     full-crosshair
                             )))
    (setf (plotter-xmin pane) 0
          (plotter-ymin pane) 0
          (plotter-xmax pane) (1- xsize)
          (plotter-ymax pane) (1- ysize)
          (plotter-box  pane) (list 0 0 (1- xsize) (1- ysize)))
    (make-instance window-class
                   :name           name
                   :title          title
                   :drawing-area   pane
                   ;; :window-styles '(:internal-borderless)
                   :layout         (make-instance 'capi:simple-layout
                                                  :description (list pane))
                   :menu-bar-items
                   (list
                    (make-instance 'capi:menu
                                   :title "Pane"
                                   :items (list
                                           (make-instance 'capi:menu-item
                                            :text          "Copy"
                                            :callback      'copy-image-to-clipboard
                                            :accelerator   "accelerator-c")
                                           (make-instance 'capi:menu-item
                                            :text          "Save as..."
                                            :callback      'save-image-from-menu
                                            :accelerator   "accelerator-s")
                                           (make-instance 'capi:menu-item
                                            :text          "Print..."
                                            :callback      'print-plotter-pane
                                            :accelerator   "accelerator-p"))
                                   :callback-type :data
                                   :callback-data-function  (constantly pane)))
                   :visible-min-width  #-:WIN32 visible-min-width #+:WIN32 (+ visible-min-width 4)
                   :visible-max-width  #-:WIN32 visible-max-width #+:WIN32 (+ visible-max-width 4)
                   :visible-min-height #-:WIN32 visible-min-height #+:WIN32 (+ visible-min-height 4)
                   :visible-max-height #-:WIN32 visible-max-height #+:WIN32 (+ visible-max-height 4)
                   :best-width         #-:WIN32 best-width #+:WIN32 (+ best-width 4)
                   :best-height        #-:WIN32 best-height #+:WIN32 (+ best-height 4)
                   :best-x             best-x
                   :best-y             best-y
                   :window-styles      window-styles)
    ))

;; ------------------------------------------
(defvar *plotter-window-class* '<plotter-window>)

(defun window (name &key
                    (title      (format nil "~A" name))
                    (background #.(color:make-gray 1))
                    (foreground #.(color:make-gray 0))
                    (width      400)
                    (height     300)
                    (xsize      width)
                    (ysize      height)
                    x
                    y
                    (xpos       x)
                    (ypos       y)
                    (best-width         xsize)
                    (best-height        ysize)
                    (best-x             xpos)
                    (best-y             ypos)
                    (visible-min-width  (/ xsize 2))
                    (visible-min-height (/ ysize 2))
                    (visible-max-width  (* xsize 2))
                    (visible-max-height (* ysize 2))
                    (cursor             (or *cross-cursor*
                                            :crosshair))
                    full-crosshair
                    (window-styles '(:internal-borderless))
                    (window-class *plotter-window-class*)
                    &allow-other-keys)

  (wclose name)
  (let* ((intf (make-plotter-window
                :name                name
                :title               title
                :best-width          best-width
                :best-height         best-height
                :visible-min-width   visible-min-width
                :visible-min-height  visible-min-height
                :visible-max-width   visible-max-width
                :visible-max-height  visible-max-height
                :best-x              best-x
                :best-y              best-y
                :background          background
                :foreground          foreground
                :cursor              cursor
                :full-crosshair      full-crosshair
                :window-styles       window-styles
                :window-class        window-class))
         (pane (drawing-area intf)))
    (capi:display intf)
    pane))

;; ------------------------------------------
(defun wset (name &rest args &key clear &allow-other-keys)
  ;; If window exists don't raise it to the top.
  ;; If window does not exist then create it with default parameters.
  ;; Return the plotting pane object
  (let ((pane (or (find-named-plotter-pane name)
                  (apply #'window name (append args *default-args*))))) ;; locate existing or create anew
    (when clear
      (clear pane))
    pane))

;; ------------------------------------------
(defun wshow (name &rest args &key clear &allow-other-keys)
  ;; If window exists then raise it to the top.
  ;; If window does not exist then create it with default parameters
  (let* ((pane (or (find-named-plotter-pane name)
                   (apply #'window name (append args *default-args*))))  ;; locate existing or create anew
         (intf (capi:top-level-interface pane)))
    (capi:execute-with-interface intf
                                 #'capi:raise-interface intf)
    (when clear
      (clear pane))
    pane))

;; ------------------------------------------
(defun wclose (name)
  ;; if window exists then ask it to commit suicide and disappear
  (let ((intf (locate-plotter-window name)))
    (when intf
      (capi:execute-with-interface intf #'capi:destroy intf))
    ))

