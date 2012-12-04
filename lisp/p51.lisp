#|
Problem 51
29 August 2003

By replacing the 1st digit of *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.

|#

(defun search-range ()
  (remove-if (lambda (x) (< x 56003)) *small-primes*))

(defun evens (list)
  "return the even indexed elements in list"
  (loop for i from 1
       for k in list
       when (evenp i)
       collect k))

(defun odds (list)
  (loop for i from 1
       for k in list
       when (oddp i)
       collect k))

(defun odd-members (n list)
  (loop for i on list
       for j from 1
       when (and (oddp j) (= n (car i)))
       return i))

(defun collect-pairs (list) ; this is an awful name for what it does
  "group elements of list by e (count e) into a new list"
  (if (null list)
      nil
      (let ((p (first list)))
	(append (list p (count p list)) (collect-pairs (remove p list))))))


(defun k-same-digits (k list)
    (loop for i in list
       when (some (lambda (p) (>= p k)) (evens (collect-pairs (digits i))))
	 collect i))

(defun k-specific-digits (k d list)
  (loop for i in list
       when (<= k (or (cadr (odd-members d (collect-pairs (digits i)))) 0))
       collect i))

;;; rather than thinking of the digits, what if we make masks and add them?
(defun summand (digit number)
  (let* ((digits (digits number))
	 (len (length digits)))
    (apply #'+
	   (mapcar (lambda (e) (expt 10 (- len e 1)))
		   (loop for i in digits 
		      for p from 0
		      when (= i digit)
		      collect p)))))

(defun six-digit-primes ()
  (remove-if-not (lambda (x) (< 100000 x 999999)) *small-primes*))

(defun matching-exactly-three ()
  (let ((sdp (six-digit-primes)))
    (loop for i in (list 0 1 2) append
	 (k-specific-digits 3 i sdp))))
  
(defun p51 ()
  (loop for i in (sort (matching-exactly-three) #'<)
					; make the bitmask
     for sum0 = (summand 0 i) for sum1 = (summand 1 i) for sum2 = (summand 2 i)
     when 
       (or ; if any of these are true, we're good
	(and (plusp sum0) ; there is a zero
	     (< 6 (count-if-not #'null ; at least seven subsequent are prime, so 8
				(mapcar 
				 (lambda (x) (member-sorted x *small-primes*)) 
				 (loop for j from 1 to 9 collect (+ i (* sum0 j)))))))
	(and (plusp sum1) 
	     (< 6 (count-if-not #'null 
				(mapcar 
				 (lambda (x) (member-sorted x *small-primes*)) 
				 (loop for j from 1 to 8 collect (+ i (* sum1 j))))))
	     (and (plusp sum2)
		  (< 6 (count-if-not #'null 
				     (mapcar 
				      (lambda (x) (member-sorted x *small-primes*)) 
				      (loop for j from 1 to 7 collect (+ i (* sum0 j)))))))))
     return i))
