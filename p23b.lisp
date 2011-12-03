(defparameter *abundants* nil)
(defparameter *hash* nil)

(defun populate-hash ()
  (setq *abundants* (abundants))
  (setq *hash* (make-hash-table :size 7000))
  (iter (for x in *abundants*)
	(setf (gethash x *hash*) t)))

(defun abundant-summable-p (n)
  (if (< n 24)
      nil
      (iter (for a in *abundants*)
	    (when (gethash (- n a) *hash*)
	      (return t))
	    (finally (return nil)))))

(defun euler23 ()
  (iter (for i from 1 to 28123)
	(unless (abundant-summable-p i)
	  (summing i))))
