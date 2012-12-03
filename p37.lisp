;; Problem 37
;; 14 February 2003

;; The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

;; Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

;; NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.


;; wanted to avoid too many calls to factor, so masked primep
;; *small-primes* is a preallocated list of primes upto some limit
(flet ((primep (n)
	 ;; whipped up a quick member for sorted lists
	 (dolist (p *small-primes*)
	   (cond ((= n p) (return  t))
		 ((> p n) (return nil))))))

  (defun left-truncate (n)
    "remove the leading digit"
    (let ((power (floor (log n 10))))
      (mod n (expt 10 power))))
  
  (defun right-truncate (n)
    "remove the trailing digit"
    (truncate n 10))  ;; use the first value only

  (defun left-truncatable-prime (p)
    "test left-trunk for left-truncatableness"
    (let ((trunk (left-truncate p)))
      (and (primep trunk)
	   (or (< trunk 10)
	       (left-truncatable-prime trunk)))))

  (defun right-truncatable-prime (p)
    "test right-trunk for right-truncatableness"
    (let ((trunk (right-truncate p)))
      (and (primep trunk)
	   (or (< trunk 10)
	       (right-truncatable-prime trunk)))))
  
  (defun truncatable-prime (p)
    "a truncatable prime is a) prime, b) left-truncatable, c) right-truncatable, d) greater than 10"
    (and (primep p)
	 (and 
	  (not (<  p 10))
	  (left-truncatable-prime p)
	  (right-truncatable-prime p))))

  (defun truncatable-primes (&optional (count 11))
    (let (result
	  (cnt 0))
    (dolist (p *small-primes*)
      (when (truncatable-prime p)
	(push p result)
	(incf cnt))
      (if (= cnt count)
	  (return result)))))
 
) ; flet primep


(defun p37 ()
  (reduce #'+ (truncatable-primes)))
