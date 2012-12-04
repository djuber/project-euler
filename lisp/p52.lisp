#|
Problem 52
12 September 2003

It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

|#

(defun six-multiples-of (x)
  (loop for i from 1 to 6 collect (* x i)))

(defun identical-digits (list-of-nums)
  (equal 
   (sort (digits (first list-of-nums)) #'<)
   (sort 
    (reduce #'intersection 
	    (mapcar #'digits (rest list-of-nums))
	    :initial-value (list 0 1 2 3 4 5 6 7 8 9)) 
    #'<)))


(defun search-blindly (lower upper)
  (loop for i from lower to upper
     when (identical-digits (six-multiples-of i))
       return i))

(defun p52 ()
  (loop for i from 1 by 100
       for k = (+ 100 i)
      when (search-blindly i k)
       return it))
