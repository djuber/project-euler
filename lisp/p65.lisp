#|
Hence the sequence of the first ten convergents for √2 are:
1, 3/2, 7/5, 17/12, 41/29, 99/70, 239/169, 577/408, 1393/985, 3363/2378, ...

What is most surprising is that the important mathematical constant,
e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].

The first ten terms in the sequence of convergents for e are:
2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...

The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.

Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.
|#

(defun nth-term-for-e-sequence (n)
  (cond ((= n 1) 2)
	((zerop (mod n 3)) (* 2 (/ n 3)))
	(t 1)))

(defun e-sequence-to (n)
  (loop for i from 1 to n collect 
       (nth-term-for-e-sequence i)))


;; this isn't working right.
(defun nth-convergent (list-of-terms)
  (if (not (cdr list-of-terms))
      (first list-of-terms)
	(do*  
	 ((terms (reverse list-of-terms) (rest terms))
	  (term (car terms) (car terms))
	  (fraction (/ 1 term)
		       (/ 1 (+  term fraction))))
	 ((null (cdr terms)) (+ term fraction))
	  (format t "term ~a fraction ~a~%" term fraction))))

;; this is
(defun nth-convergent-of-e (n)
  (if (= n 1) 
       2
       (do* 
	((term n (1- term))
	 (fraction (/ (nth-term-for-e-sequence term))
		   (/ (+ (nth-term-for-e-sequence term) fraction))))
	((= term 2) (+ 2 fraction)))))

	
(defun p65 ()
  (loop for i across (format nil "~a" (numerator (nth-convergent-of-e 100)))
       summing (- (char-code i) (char-code #\0))))

