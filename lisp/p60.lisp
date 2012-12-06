#|
Problem 60
02 January 2004

The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.

Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

|#

;; for a singleton, 2 is the correct answer
;; for a pair, I think 7 and 3 are right (37, 73 both prime)

;; for a set of four, we are given 3,7,109, and 673
;; every subset of a concatenable set is concatenable, does that help us?


;; make some tools : 

(defun proper-pairs (set)
  "given a list of numbers, make a list of two element lists with each item different"
  (let ((crosses (cross set set)))
    (remove-if 
     (lambda (pair) 
       (= (first pair) (second pair))) 
     crosses)))

(defun number-of-digits (n)
  (1+ (floor (log n 10))))

(defun concatenate-numbers (pair)
  (let ((n (first pair))
	(m (second pair)))
    (+ (* n (expt 10 (number-of-digits m))) m))))

(defun concatenable (set-of-primes)
  "a set of primes is concatenable if every pair of elements can concatenate to form a prime"
  (every #'m-r-check
	 (mapcar #'concatenate-numbers 
		 (proper-pairs set-of-primes))))

(defun find-concatenable-set (size)
  (let ((*small-primes* (remove-if (lambda (x) (> x 10000)) *small-primes*)))
    ;; the 10,000 bound was found by gradually increasing the bound. First to yield a result
  (labels 
      ((findit (size-left partial-set)
	 ;; try to find a prime such that this prime can be added to the partial set
	 ;; and the set can be finished
	 (if (zerop size-left)
	     partial-set
	     (dolist (i *small-primes*)
	       (when (and
		      (not (member i partial-set))
		      (concatenable (cons i partial-set)))
		 (let ((try (findit (1- size-left) (cons i partial-set))))
		   (when try
		     (return try))))))))
    (findit size nil))))


(defun p60 ()
  (apply #'+ (find-concatenable-set 5)))
