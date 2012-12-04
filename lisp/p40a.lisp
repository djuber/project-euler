;; phil_g   (LISP)  phil_g is from USA

(defun p40 ()
  (reduce #'*
	  (loop for i = 1 then (1+ i)
             with sub = 1
             append (loop for j in (digits i)
		       when (multiple-value-bind (q r)
				(truncate (log sub 10))
			      (zerop r))
		       append (list j)
		       do (incf sub))
             while (<= sub 1000000))))