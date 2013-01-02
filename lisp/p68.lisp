#|
Problem 68

Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6, and each line adding to nine.

Working clockwise, and starting from the group of three with the numerically lowest external node (4,3,2 in this example), each solution can be described uniquely. For example, the above solution can be described by the set: 4,3,2; 6,2,1; 5,1,3.

It is possible to complete the ring with four different totals: 9, 10, 11, and 12. There are eight solutions in total.
Total	Solution Set
9	4,2,3; 5,3,1; 6,1,2
9	4,3,2; 6,2,1; 5,1,3
10	2,3,5; 4,5,1; 6,1,3
10	2,5,3; 6,3,1; 4,1,5
11	1,4,6; 3,6,2; 5,2,4
11	1,6,4; 5,4,2; 3,2,6
12	1,5,6; 2,6,4; 3,4,5
12	1,6,5; 3,5,4; 2,4,6

By concatenating each group it is possible to form 9-digit strings; the maximum string for a 3-gon ring is 432621513.

Using the numbers 1 to 10, and depending on arrangements, it is possible to form 16- and 17-digit strings. What is the maximum 16-digit string for a "magic" 5-gon ring?

|#
;; for a 16-digit answer, 10 must be an outer node, not on the inner cycle.

;;; it seems like the geometric presentation is a bit of a ruse

;;; we can easily redraw the question as
#|
out/in
E  F
A  G
B  H
C  I
D  J

We fix a ten on the outside (need 16 digit string) , in place E
and require E+ F + G =  A + G + H = B + H + I = C + I + J = D + J + F
and F maximized
|#

(defun duplicated (elt list)
  "is elt duplicated in list?"
  (member elt 
	  (rest (member elt list))))

(defun no-duplicates (list)
  "every element in list is unique."
  (loop for i in list
       never (duplicated i list)))


(defun magic-5gon? (list-of-ten-numbers)
  "test a list of ten numbers for magic"
  (destructuring-bind (E A B C D F G H I J)
      list-of-ten-numbers
    (and 
     ; all sums on lines must be equal
     (= 
      (+ E F G)
      (+ A G H)
      (+ B H I)
      (+ C I J)
      (+ D J F))
     ; no number can be repeated
     (no-duplicates list-of-ten-numbers))))


(defun generate-5gons ()
  "generate 9! lists of 10 numbers, not all magic, with 10 on the outside"
  ;; every generate 5gon of this form has a 16 digit string
  (let ((digits (list 1 2 3 4 5 6 7 8 9)))
    (loop for i1 in digits append
	 (loop with digits = (remove i1 digits)
	    for i2 in digits append
	      (loop with digits = (remove i2 digits)
		 for i3 in digits append
		   (loop with digits = (remove i3 digits)
		      for i4 in digits append
			(loop with digits = (remove i4 digits)
			   for i5 in digits append
			     (loop with digits = (remove i5 digits)
				for i6 in digits append
				  (loop with digits = (remove i6 digits)
				     for i7 in digits append
				       (loop with digits = (remove i7 digits)
					  for i8 in digits append
					    (loop for i in (remove i8 digits)
						 collect 
						 (list 10 i1 i2 i3 i4 i5 i6 i7 i8 i))))))))))))

(defun rotate-right (5gon)
  "spin graph 1 position to the right, sending E->A, F->G"
  (destructuring-bind (E A B C D F G H I J)
      5gon
    (list A B C D E G H I J F)))

(defun 5gon-string (5gon)
  (let ((smallest-outer (apply #'min (subseq 5gon 0 5))))
    (if (not (= (first 5gon) smallest-outer))
	(5gon-string (rotate-right 5gon))
	(destructuring-bind (E A B C D F G H I J)
	    5gon
	  (let ((string-list (list E F G A G H B H I C I J D J F)))
	    (read-from-string
	     (format nil "~{~a~}" string-list)))))))
	
(defun p68 ()
  (loop for 5gon in (remove-if-not #'magic-5gon? (generate-5gons))
       maximizing (5gon-string 5gon)))
