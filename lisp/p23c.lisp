(defun find-divisors (x)
  (let ((divs nil))
    (loop for i from 1 to (sqrt x) 
       do (when (zerop (mod x i)) 
	    (push i divs)
	    (let ((d (/ x i)))
	      (unless (or (= i 1) (= i d))
		(push d divs)))))
    divs))
 
(defun abundant-p (x)
  (> (reduce #'+ (find-divisors x)) x))
 
(defun euler-proj-23 (&optional (size 28123))
  (let ((sums-of-abundants (make-array size :element-type 'bit))
	(abundants nil))
    (loop for i from 0 to size
	 do (when (abundant-p i)
	      (setq abundants (append abundants (list i)))
	      (loop for j in abundants
		 while (< (+ i j) size)
		 do (setf (elt sums-of-abundants (+ i j)) 1))))
    (loop for i from 0 below size
	 when (zerop (elt sums-of-abundants i))
	 sum i)))
