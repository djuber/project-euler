;; Problem 39
;; 14 March 2003

;; If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

;; {20,48,52}, {24,45,51}, {30,40,50}

;; For which value of p ≤ 1000, is the number of solutions maximised?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; observe that the perimeter must be even (if a and b were odd,
;; so are their squares, so their sum would be even, and so would c
;; if a were odd and b even, c^2 would be odd, and so would c
;; if a b both even, c is even

;; 2nd observation: if 12 is a valid perimeter (3 4 5), so are all integer multiples of 12 (24, 36, etc), so maybe this breaks down to finding the product of the most
;; unique cases (3,4,5), (5, 12, 13), (8, 15, 17), etc.
;;  12 30 40 56 70  84 90 126 154 176 182 198 208 220 234 260 286 306 340
;;  I think it may be possible that 2 primes will occur in any triple, but this may
;; be totally unjustified.

;; this problem could be translated to genarate all pythagorean triples
;; with c less than 5000 with a + b + c less than 10000, and finding
;; the number less than 10000 with the most divisors among the sums?

;; don't think this matters much, but why not
(declaim (optimize (speed 3) (safety 0)))
(defun sqr (n)
  (declare (type fixnum n))
  (let ((a (* n n)))
    (the fixnum a)))

(defun pythagorean (a b c)
  "determine if a^2 + b^2 = c^2"
  (declare (type integer a b c))
  (= (sqr c) (+ (sqr b) (sqr a))))

(defun perimeterp (perimeter a b c)
  (declare (type integer perimeter a b c))
  (= perimeter (+ a b c)))
	    

;; this is entirely too slow, and brute force search will be fruitless

(defun sides (perimeter)
  "return the sides of a right triangle with perimeter"
  (declare (type integer perimeter))
  (loop for i from 2 to (1- perimeter)
	summing (length (loop 
			 for j from 1 to i
			 if (loop 
			     for k from 1 to j
			     when (and (perimeterp perimeter i j k)
				       (pythagorean j k i))
			     return t)
			 collect j))))

#|
;; this chunk essentially spun in circles... need to rethink
;; 24+ hours on i7 and it was looking at 3783
(loop for i from 1 to 10000
      collect (format nil "~a ~a~%" i (sides i)))
|#