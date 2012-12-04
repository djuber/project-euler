#|
Problem 53
26 September 2003

There are exactly ten ways of selecting three from five, 12345:

123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

In combinatorics, we use the notation, 5C3 = 10.

In general,
nCr = 	
n!
r!(n−r)!
	,where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.

It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.

How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100, are greater than one-million?

|#

(defun choose (n r)
  (cond 
    ((= n 0) 0)
    ((= r 0) 0)
    ((= r 1) n)
    ((= r n) 1)
    ((> r n) 0)
    (t 
     (/ (reduce #'* (loop for i from (1+ (- n r)) to n collect i))
	(! r)))))

(defun p53 ()
  (loop for i from 23 to 100 summing
       (loop for j from 1 to i
	    when (< 1000000 (choose i j))
	    count j)))
