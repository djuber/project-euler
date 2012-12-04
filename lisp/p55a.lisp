;; Grue   (LISP)  Grue is from Russia
(defun euler-55 ()
  (flet ((num-reverse (n)
	   (loop with rev = 0
	      for (q r) = (multiple-value-list (floor n 10))
	      then (multiple-value-list (floor q 10))
	      do (setf rev (+ (* rev 10) r))
	      until (zerop q)
	      finally (return rev))))
    (loop for x from 1 to 10000
       if (loop for i from 1 to 50
	     for a = (+ x (num-reverse x)) then (+ a b)
	     for b = (num-reverse a)
	     never (= a b))
       collect x)))
