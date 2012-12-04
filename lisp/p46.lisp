;; It was proposed by Christian Goldbach that every odd composite number
;; can be written as the sum of a prime and twice a square.

;; 9 = 7 + 2×1^(2)
;; 15 = 7 + 2×2^(2)
;; 21 = 3 + 2×3^(2)
;; 25 = 7 + 2×3^(2)
;; 27 = 19 + 2×2^(2)
;; 33 = 31 + 2×1^(2)

;; It turns out that the conjecture was false.

;; What is the smallest odd composite that cannot be written as the
;; sum of a prime and twice a square?

;; abort if we've gone too far
(defun member-sorted (n list)
  "is n in list, when list is a sorted increasing sequence of integers"
  (dolist (x list)
    (when (= x n)
      (return t))
    (when (> x n)
      (return nil))))

(defun upto-sorted (n list)
  "collect elements of list less than or equal to n"
  (let (result)
    (dolist (x list)
      (if (<= x n)
	(push x result)
	(return (nreverse result))))))

(defun odd-composites (limit)
  (loop for i from 9 to limit by 2
     when (not (member-sorted i *small-primes*))
       collect i))

(defun squares (index-limit)
  (loop for i from 1 to index-limit collect (square i)))

(defun sum-of-prime-and-twice-a-square (n)
  (dolist (x (squares (isqrt n)))
    (when (member-sorted (- n (* 2 x)) *small-primes*) 
	(return T))))
    
(defun p46 ()
  (first (loop for c in (odd-composites 10000)
	    when (not (sum-of-prime-and-twice-a-square c))
	      collect c)))
