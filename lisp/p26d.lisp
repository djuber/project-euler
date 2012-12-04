#|
30 Jun 2007 04:28 pm 
msingh
LISP 
Australia	
   Quote    0
|#
(defun relatively-prime-p (a b)
  (= (gcd a b) 1))

(defun multiplicative-order (n)
  (if (relatively-prime-p n 10)
      (iter (for k from 1)
	    (for a = (expt 10 k))
	    (for m = (mod a n))
	    (if (= m 1) (return k))
	    (finally (return 0))) 0))

(defun regulars (&key alpha beta max)
  (remove-if (lambda (x) (> x max))
	     (sort (iter (for i from 0 to alpha)
			 (unioning
			  (iter (for j from 0 to beta)
				(collect (* (expt 2 i) (expt 5 j)))))) #'<)))

(defun euler26 ()
  (let* ((regulars (regulars :alpha 10 :beta 5 :max 1000))
	 (space (iter (for i from 1 to 1000)
		      (unless (find i regulars)
			(collect i)))))
    (iter (for x in space)
	  (finding x maximizing (multiplicative-order x)))))