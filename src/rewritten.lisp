(let* ((tree (car 
	      (labels ((iter (nodes)
			 (if (= 1 (length nodes))
			     nodes
			     (iter (mapcar 'make-tree-node (um:group nodes 2))))))
		(iter nodes)))))
  (when tree
    (labels ((iter (tree)
	       (if (tree-node-p tree)
		   (let* ((l (tree-node-l tree))
			  (r (tree-node-r tree))
			  (tstake (node-stake tree)))
		     (if (< (/ lstake tstake) nfrac)
			 (iter r)
			 (iter l)))
		   (first tree))))
      (iter tree))))
