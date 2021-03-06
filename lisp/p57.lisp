#|
Problem 57
21 November 2003

It is possible to show that the square root of two can be expressed as an infinite continued fraction.

√ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

By expanding this for the first four iterations, we get:

1 + 1/2 = 3/2 = 1.5
1 + 1/(2 + 1/2) = 7/5 = 1.4
1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.

In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?

|#

(defun p57 ()
  (loop for i from 1 to 1000 
     for fraction = (nth-continued-fraction-for-root-two i)
     when (top-heavy fraction)
       count i))
       
(defun top-heavy (fraction)
  "a top-heavy fraction has more digits above than below"
  (> (number-of-digits (numerator fraction))
     (number-of-digits (denominator fraction))))

(defun number-of-digits (number)
  "use a useful fact about log(n)"
  (1+ (floor (log number 10))))

(defun nth-continued-fraction-for-root-two (n)
  (labels ((doit (n)
	     (if (> n 1)
		 (/ 1 (+ 2  (doit (1- n))))
		 1/2)))
    (1+ (doit n))))
