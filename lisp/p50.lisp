#|

Problem 50
15 August 2003

The prime 41, can be written as the sum of six consecutive primes:
41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?

|#


(defun p50 ()
  (reduce #'+ (second (first 
  (let ((tiny-primes (upto-sorted 1000000 *small-primes*)))
    (loop for k from (find-upper-bound-for-problem-50) downto 30 when 
	 (loop for head on tiny-primes
	    while (< (car head) 50000) ; 1M / 21
	    when 
	      (let ((x (apply #'+ (subseq head 0 k))))
		(and (< x 1000000) (member-sorted x tiny-primes)))
	    collect (list k (subseq head 0 k)))
       return it))))))
	    
     
(defun find-upper-bound-for-problem-50 ()
  (loop for i in *small-primes*
       summing i into sum
       counting i into count
       when (> sum 1000000)
       return count))

