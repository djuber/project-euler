#|
Problem 58
05 December 2003

Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.

37 36 35 34 33 32 31
38 17 16 15 14 13 30
39 18  5  4  3 12 29
40 19  6  1  2 11 28
41 20  7  8  9 10 27
42 21 22 23 24 25 26
43 44 45 46 47 48 49

It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.

If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?

|#

(defun primes-in (lower upper)
  (remove-if-not (lambda (x) (< lower x upper)) *small-primes*))

(defun outer-ratio (step)
  (/ (length (primes-in (square (1- step)) (square step))) (* step (1- step))))

(defun when-does-outer-ratio-fall-below-ten-percent ()
  "solved the wrong problem"
  (loop for i from 2
       when (> 1/10 (outer-ratio i))
       return i))

;; want corners 1 -> (1)
;; corners 2 -> (3 5 7 9)
;; corners 3 -> (13 17 21 25)

(defun spiral-diagonals (steps)
  (loop for i from 1 to steps append (corners i)))

(defun spiral-ratio (n)
    (let ((diags (spiral-diagonals n)))
      (/ (length (intersection diags *small-primes*)) (length diags))))

(defun step-results (n)
  (length (prime-corners n)))

(defun p58 ()
  (1+ ;; sides are 2n + 1 
   (* 2  
      (1+ ;; skipped 1, so increase count
	 (loop for i from 2 ; count the iterations
	    for j = 5 then (+ 4 j) ; count the diagonal elements
	    summing (step-results i) into sum ; number of new primes
	    while (> (/ sum j) 1/10)
	    count i)))))

(defun member-sorted (x list)
  (dolist (r list)
    (if (>= r x)
	(if (= x r)
	   (return T)
	   (return nil))
	nil)))

(let ((primes (sieve5 (expt 10 9)))) ; this eats a lot of memory!
  (defun prime-corners (n)
    (let* ((corners (corners n))
	   (result
	    (loop for i in corners
	       when (member-sorted i primes)
	       collect i)))
      (when result ; we want to eliminate low numbers, so strip of matching heads
	(setf primes (member (last1 result) primes)))
      result))

  (defun reset-pool (&optional (limit (expt 10 9)))
    (setf primes (sieve5 limit)))

  (defun check-prime-pool ()
    (values
     (first primes)
     (last1 primes))))

(defun corners (n)
  (if (= 1 n)
      (list 1)
      (let ((base  (square  (1- (* 2 n))))
	    (offset (- (* 2 (1- n)))))
	(loop for i from 0 to 3 collect 
	     (+ base (* i offset))))))
    
