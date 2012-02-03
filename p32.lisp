#|
Problem 32
06 December 2002

We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

|#

(defun unique-digits (a b c)
  "true when a b and c have no common digits"
  (let (digits)
    (loop for i in (list a b c)
       do (loop for j in (digits i)
	     do (pushnew j digits)))
    (setf digits (sort digits #'<))
    (values
     (and (not (member 0 digits))
	  (equal (sort (append (digits a)
			       (digits b)
			       (digits c)) #'<)
		 digits))
     (length digits))))

(defun pandigital-triple (a b c)
  (let (unique 
	len)
    (declare (ignorable unique)
	     (ignorable len)
    (multiple-value-bind (unique len) (unique-digits a b c)
      (and unique
	   (= len 9)))))

(defun p32 ()
  (let ((products nil))
    (loop for i from 2 to 99
	 do (progn 
	      (loop for j from i to 1999
		 do (let ((k (* i j)))
		      (when (pandigital-triple i j k)
			(pushnew k products))))))
    (values
     products
    (apply #'+ products))))
