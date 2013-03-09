
(defun problem71 ()
  (numerator
   (let ((best 2/7))
     (do ((denom 8 (1+ denom)))
	 ((= denom (expt 10 6)) best)
       (loop for number from 
	    (floor 
	     (/ 
	      (* (numerator best) denom) 
	      (denominator best)))
	  for fraction = (/ number denom)
	  until (>= fraction 3/7)
	  do  (setf best (max best fraction)))))))
