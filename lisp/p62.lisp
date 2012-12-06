#|
Problem 62
30 January 2004

The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits are cube.


|#



(defun p62 ()
  (expt (apply #'min (p62-driver)) 3))

(defun p62-driver ()
  (let (( cube-digits (make-hash-table :test #'equal))
	( cube-digits-smallest (make-hash-table :test #'equal)))
  (loop for digits from 6
       for lower = (floor (expt (expt 10 digits) 1/3))
       for upper = (ceiling (expt (expt 10 (1+ digits)) 1/3))
       with results = '()
       do
       (loop for i from lower to upper  
	  for d = (sort (digits (* i i i)) #'<)
	  do
	    (if (gethash d cube-digits)
		(case (incf (gethash d cube-digits))
		  (5 (push d results))
		  (6 (setf results (delete d results))))
		(setf (gethash d cube-digits) 1 (gethash d cube-digits-smallest) i)))
       when results
       return (mapcar (lambda (r) (gethash r cube-digits-smallest)) results))))
		     
