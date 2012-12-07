#|
Problem 63
13 February 2004

The 5-digit number, 16807=75, is also a fifth power. Similarly, the 9-digit number, 134217728=89, is a ninth power.

How many n-digit positive integers exist which are also an nth power?

|#

(defun digit-span (n)
  "lower and upper bounds for numbers of n digits"
  (list (expt 10 (1- n))
	(1- (expt 10 n))))

(defun nth-powers-with-n-digits (n)
  (let ((upper (second (digit-span n)))
	(lower (first (digit-span n))))
  (loop for i from 1
       for pow = (expt i n)
       while (<= pow upper)
       when (<= lower pow upper)
       collect pow)))

(defun p63 ()
  (loop for n from 1 to 21 ; magic number found by experiment
     summing (length (nth-powers-with-n-digits n))))

;; answer 49


