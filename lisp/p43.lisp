#|

Problem 43
09 May 2003

The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

    d2d3d4=406 is divisible by 2
    d3d4d5=063 is divisible by 3
    d4d5d6=635 is divisible by 5
    d5d6d7=357 is divisible by 7
    d6d7d8=572 is divisible by 11
    d7d8d9=728 is divisible by 13
    d8d9d10=289 is divisible by 17

Find the sum of all 0 to 9 pandigital numbers with this property.

|#

(defun p43 ()
  (loop for pan in (all-zero-to-nine-pandigitals)
       when (satisfies-p43 pan)
       sum (digits-to-number pan)))

(defun satisfies-p43 (list)
  (and 
   (evenp (fourth list))
   (zerop (mod (reduce #'+ (subseq list 2 5)) 3))
   (zerop (mod (sixth list) 5))
   (zerop (mod (digits-to-number (subseq list 4 7)) 7))
   (zerop (mod (digits-to-number (subseq list 5 8)) 11))
   (zerop (mod (digits-to-number (subseq list 6 9)) 13))
   (zerop (mod (digits-to-number (subseq list 7 10)) 17))))
	  


(defun equal-any (n list)
  (some (lambda (k) (= n k)) list))

;; remove simple wrong answers 
(defun all-zero-to-nine-pandigitals ()
  (loop for d1 from 1 upto 9 append ; skip leading zero
       (loop for d2 upto 9
	    with used = (list d1)
	    when (not (equal-any d2 used)) 
	  append
	    (loop for d3 upto 9
		 with used = (cons d2 used)
	       when (not (equal-any d3 used))
	       append
		 (loop for d4 in '(0 2 4 6 8) ; only even numbers here
		      with used = (cons d3 used)
		    when (not (equal-any d4 used))
		    append 
		      (loop for d5 upto 9
			 with used = (cons d4 used)
			 when (and (not (equal-any d5 used))
				   (zerop (mod (+ d3 d4 d5) 3))) ; 3 | sum of digits
				   append
				   (loop for d6 in '(0 5) ; only multiples of five here
				      with used = (cons d5 used)
				      when (not (equal-any d6 used))
				      append
					(loop for d7 upto 9
					   with used = (cons d6 used)
					   when (not (equal-any d7 used))
					   append
					     (loop for d8 upto 9
						with used = (cons d7 used)
						when (and (not (equal-any d8 used))
							  (zerop (mod (+ d6 (- d7) d8) 11))) 
						;; 11 divides alternating sum
						append 
						  (loop for d9 upto 9
						     with used = (cons d8 used)
						     when (not (equal-any d9 used))
						     append
						       (loop for d10 upto 9
							  with used = (cons d9 used)
							  when (not (equal-any d10 used))
							  collect (list d1 d2 d3 d4 d5 d6 d7 d8 d9 d10))))))))))))
						    


