#|
Problem 49
01 August 2003

The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?
|#

(defvar *primes* 
  (loop for i from 1000 to 10000
     when (primep i)
       collect i))

(defun p49 ()
  "print combinations of four digit primes in arithmetic sequence which are permutations of each other."
  (let ((primes (loop for i from 1000 to 10000
		     when (primep i)
		     collect i)))
    (loop for i in (member 1487 primes)
       do (loop for j in (rest (member i primes))
	     do (loop for k in (rest (member j primes))
		   when 
		     (and 
		      (= (- k j) (- j i))
		      (equalp (sort (digits i) #'<)
			      (sort (digits j) #'<))
		      (equalp (sort (digits j) #'<)
			      (sort (digits k) #'<)))
		   do (format t "~a~a~a~%" i j k))))))