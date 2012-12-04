#|
Problem 39
14 March 2003

If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?

|#

;; first, we know that for every primitive pythagorean triple
;; there is an m,n with m>n>0, m, n one odd, one even, 
;; and a = m^2 - n^2, b = 2mn, c = m^2 + n^2

;; can we generate all pythagorean triples with perimeter less than 1000?

(defun perimeter (a b c)
  (+ a b c))

(defun triples (&optional (perimeter 1000))
  "generate all pythagorean triples with perimeter upto perimeter."
  (let ((mn-limit (isqrt perimeter))) ; what is the right upper bound here?
  (loop for n from 1 to (1- mn-limit) append
       (loop for m from (1+ n) to mn-limit by 2
	    append 
	    (let ((a (- (* m m) (* n n)))
		  (b (* 2 m n))
		  (c (+ (* m m) (* n n)))
		  result)
	      ;; here we have a primitive pythagorean triple, and want to collect
	      ;; all the multiples of this with perimeter upto the bound
	      (do ((x a (+ x a))
		   (y b (+ y b))
		   (z c (+ z c)))
		  ((> (perimeter x y z) perimeter) result)
		(push (list x y z) result)))))))
	     

(defun p39 ()
  (let ((max-count 1)
	(best-value nil))
  (loop for c on 
	(mapcar #'(lambda (triple) (apply #'perimeter triple)) (triples 1000))
     when (< max-count (count (car c) c))
       do (progn (setf max-count (count (car c) c))
		 (setf best-value (car c))))
  best-value))



