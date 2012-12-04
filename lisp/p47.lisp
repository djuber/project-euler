;; Problem 47
;; 04 July 2003

;; The first two consecutive numbers to have two distinct prime factors are:

;; 14 = 2 × 7
;; 15 = 3 × 5

;; The first three consecutive numbers to have three distinct prime factors are:

;; 644 = 2² × 7 × 23
;; 645 = 3 × 5 × 43
;; 646 = 2 × 17 × 19.

;; Find the first four consecutive integers to have four distinct primes factors. What is the first of these numbers?


(defun number-of-prime-factors (n)
  (/ (length (prime-powers n)) 2))

(defun numbers-with-k-distinct-prime-factors (k limit)
  (loop for i from 2 to limit
       when (= k (number-of-prime-factors i))
       collect i))

(defun smallest-consecutive-members (n list)
  (loop for i in list
       when (every (lambda (e) (member-sorted e list)) (loop for e from i to (1- (+ i n)) collect e))
collect i))

(defun p47-test-samples ()
  (values
   (first (smallest-consecutive-members 2
					(numbers-with-k-distinct-prime-factors 2 100)))
   (first (smallest-consecutive-members 3 
					(numbers-with-k-distinct-prime-factors 3 1000)))))

(defun p47 ()
  (first (smallest-consecutive-members 4 
				       (numbers-with-k-distinct-prime-factors 4 200000))))
