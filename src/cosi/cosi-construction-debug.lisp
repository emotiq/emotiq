(in-package :emotiq/cosi)

(defmethod children (x layout)
  nil)
  
(defmethod children ((node node:node) layout)
  (remove nil (coerce (node-other-members node) 'list)))

(defmethod realnode ((node node:node))
  (string-equal (node:ip node) (node:real-ip node)))

(defun split-to-octets (val)
  (um:nlet-tail iter ((n   4)
                      (val val)
                      (lst nil))
                (if (zerop n)
                    lst
                    (iter (1- n) (ash val -8) (cons (ldb (byte 8 0) val) lst)))))

(defclass red-text (capi:item-pinboard-object)
  ())

(defclass black-text (capi:item-pinboard-object)
  ())

(defmethod capi:draw-pinboard-object :around (pinboard (self red-text) &key &allow-other-keys)
  (gp:with-graphics-state (pinboard :foreground :red)
    (call-next-method)))
  
(defmethod make-node-pane (graphics-port (node node:node))
  (declare (ignore graphics-port))
  (let ((txt (node:ip node)))
    (make-instance (if (realnode node)
                       'red-text
                       'black-text)
                   :text txt)))

(defmethod view-tree ((tree node:node) &key (layout :left-right))
  (unless (emotiq:x11-display-p)
    (pr "No X11 display available to view tree")
    (return-from view-tree nil))
  (capi:contain
   (make-instance 'capi:graph-pane
                  :layout-function layout
                  :roots (list tree)
                  :node-pane-function 'make-node-pane
                  :children-function (lambda (node)
                                       (children node layout)))))
