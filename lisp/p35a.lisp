

(defun euler-035 (n)
  "How many circular primes below N? (N = 1,000,000)"
  (let* ((primes (erat n))
	 (circ-primes (loop for prime in primes
			 when (circular-prime-p prime)
			 collect prime))
	 (num-circ-primes (length circ-primes)))
    (values num-circ-primes circ-primes)))



(defun circular-prime-p (n)
  "Return T if N is a circular prime (all digit rotations are prime.
Note that N is assumed prime."
  (let* ((digits (digit-list n))
	 (num-digits (length digits)))
    (do* ((count 1 (1+ count))
	  (rot (rotate-list digits) (rotate-list rot))
	  (circularp t (primep (digit-list->integer rot))))
	 ((or (not circularp)
	      (> count num-digits))
	  circularp))))



(defun digit-list->integer (list)
"Convert base 10 digit LIST into an integer."
(do* ((digits (reverse list))
      (power 0 (1+ power))
      (dig-list digits (cdr dig-list))
      (value (first dig-list) (if dig-list
				  (+ value
				     (* (first dig-list)
					(expt 10 power)))
				  value)))
     ((null dig-list) value)))



(defun rotate-list (list &optional (n 1))
  "Return LIST rotated left by N."
  (do ((count 0 (1+ count))
       (rot list (append (cdr rot)
			 (list (first rot)))))
      ((>= count n) rot)))



(defun digit-list (n)
  "Return a list of the digits in integer N."
  (loop for c across (format nil "~A" n)
     collect (char-to-val c)))



(defun erat (n)
"Return a list of prime numbers less than N using the Sieve of Eratosthenes."
(assert (and (> n 2)
	     (< n array-dimension-limit)))
(let ((primes-list (list))
      
      ;; The zeroth and first elements of primep table are ignored.
      (primep (make-array n :initial-element t))
      (prime-idx 2))
  (loop while (< prime-idx n) do
       (let ((step prime-idx)
	     (search-start-idx (* prime-idx prime-idx)))
	 ;; Positioned on a prime, so add it to the list.
	 (setf primes-list (cons prime-idx primes-list))
	 ;; Mark multiples of found prime as not prime.
	 (loop for search-idx from search-start-idx by step
	    while (< search-idx n) do
	      (setf (aref primep search-idx) nil))
	 ;; Position prime pointer onto next prime
	 (incf prime-idx)
	 (loop while (< prime-idx n)
	    while (not (aref primep prime-idx)) do
	      (incf prime-idx))))
  (nreverse primes-list)))



(defun primep (n)
  "Return T if N is prime."
  (cond ((not (integerp n)) nil)
	((integerp (/ n 2)) (if (= n 2) t nil))
	((integerp (/ n 3)) (if (= n 3) t nil))
	((integerp (/ n 5)) (if (= n 5) t nil))
	((position n (erat 100)) t)
	(t
	 (let ((limit (isqrt n))
	       (divisor-found nil))
	   (loop for a from 31 to (+ limit 31) by 30
	      for b from 7 to (+ limit 7) by 30
	      for c from 11 to (+ limit 11) by 30
	      for d from 13 to (+ limit 13) by 30
	      for e from 17 to (+ limit 17) by 30
	      for f from 19 to (+ limit 19) by 30
	      for g from 23 to (+ limit 23) by 30
	      for h from 29 to (+ limit 29) by 30
	      until divisor-found
	      finally (return (not divisor-found))
	      when (or (integerp (/ n a))
		       (integerp (/ n b))
		       (integerp (/ n c))
		       (integerp (/ n d))
		       (integerp (/ n e))
		       (integerp (/ n f))
		       (integerp (/ n g))
		       (integerp (/ n h)))
	      do (setf divisor-found t))))))



(defparameter char-digits
  '((#\0 0)
    (#\1 1)
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)
    (#\9 9)))



(defun char-to-val (char)
  (second (assoc char char-digits)))

