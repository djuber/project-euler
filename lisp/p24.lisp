#|

Problem 24
16 August 2002

A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

|#

;; ! is good old fashioned factorial
;; since I never needed this, it's not clear why it's still here
(defun permute (n k)
  "calculate P(n,k)"
  (/ (! n)
     (! (- n k))))

(defun choose (n k)
  "calculate C(n,k)"
  (/ (! n)
     (! k) (! (- n k))))

;; if we just look at moving 1-9, with 0 fixed there are
;; permute (9 9) => 362880 possibilities.
;; rolling through these 2 times brings us to 725760
;; so the first digit is a two
;; our new limit is 274240 (1M - 725760)
;; and our digit list is (0 1 3 4 5 6 7 8 9)

(defun nth-lexicographic-permutation (n ordered-symbol-list)
  " given an alphabet, produce the nth permutation. This is zero offset"
  (if (zerop n)
      ordered-symbol-list
      (let ((remaining (length ordered-symbol-list))
	    index
	    (remainder n))
	(declare (ignorable index))
	(multiple-value-bind (index remainder) (truncate remainder (! (1- remaining)))
	  (cons (nth index ordered-symbol-list)
		(nth-lexicographic-permutation 
		 remainder 
		 (remove (nth index ordered-symbol-list) ordered-symbol-list)))))))

(defun p24 ()
  (nth-lexicographic-permutation (1- (expt 10 6)) (list 0 1 2 3 4 5 6 7 8 9)))
