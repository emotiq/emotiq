;;; gossip-stats.lisp

(in-package :gossip)

(defun count-alives (n &optional gossip-number)
  "Create a 1000-node network. Run :count-alive on
   it n times starting at random nodes. Collect statistics
   on answers received."
  (make-graph 1000)
  (let ((results nil)
        (*log-filter* nil))
    (run-gossip-sim)
    (set-protocol-style :gossip gossip-number)
    (dotimes (i n)
      (push (solicit-wait (random-node) :count-alive) results))
    (mapcar (lambda (x) (if (numberp x) x nil)) results))) ; ensure :TIMEOUT doesn't appear in data

; (count-alives 100)
; (count-alives 100 2)

; (cl-user::charts)
(defun graph-it (name data)
  (charts:make-chart-window
   :style :line
   :caption name
   :y-data (list data)
   :x-axis-name "Experiment"
   :notes (list "1000 nodes; gossip mode")))

; (graph-it "Count-Alives 1" (count-alives 100 1))
; (time (graph-it "Count-Alives 2" (count-alives 100 2)))
