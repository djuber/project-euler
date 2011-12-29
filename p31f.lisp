#|
13 Feb 2008 06:30 pm 
rjminniear
LISP  
	

A recursive Common Lisp solution:
|#

(defparameter *coins* '(200 100 50 20 10 5 2 1))

(defun euler31 (n)
  (use-coin n *coins*))

(defun use-coin (total-value coins)
  (cond ((= (length coins) 1) 1)
        (t (loop for i from 0
                 when (< total-value (car coins))
                   do (return (+ sum (use-coin total-value (cdr coins))))
                 when (zerop total-value)
                   do (return (1+ sum))
                 summing (use-coin total-value (cdr coins)) into sum
                 do (decf total-value (car coins))))))