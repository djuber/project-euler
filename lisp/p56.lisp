#|
Problem 56
07 November 2003

A googol (10100) is a massive number: one followed by one-hundred zeros; 100100 is almost unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.

Considering natural numbers of the form, ab, where a, b < 100, what is the maximum digital sum?


|#


;; wow, took a few seconds to write, and a second to run.
(defun p56 ()
  (loop for i from 2 upto 100 maximizing
       (loop for j from 2 upto 100 maximizing
	    (apply #'+ (digits (expt i j))))))
