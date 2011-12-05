;; the set of permutations of a number is isomorphic
;; to the hamiltonian paths in a graph with the digits
;; as nodes

(defun ham-trav (alist &optional (traveled '()) (func #'(lambda (x) (format t "~A~%" x))))
  "generates all permutations in a sequence  by traversing all hamiltonian paths in a complete graph,
and applies any function passed"
  (if (null alist)
      (funcall func traveled)
      (dolist (x alist)
	(let ((newt (cons x traveled)))
	  (ham-trav (set-difference alist newt) newt func)))))
