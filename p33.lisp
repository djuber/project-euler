
;; The fraction ^(49)/_(98) is a curious fraction, as an inexperienced
;; mathematician in attempting to simplify it may incorrectly believe
;; that ^(49)/_(98) = ^(4)/_(8), which is correct, is obtained by
;; cancelling the 9s.

;; We shall consider fractions like, ^(30)/_(50) = ^(3)/_(5),
;; to be trivial examples.

;; There are exactly four non-trivial examples of this type of fraction,
;; less than one in value, and containing two digits in the numerator and
;; denominator.

;; If the product of these four fractions is given in its lowest common terms,
;; find the value of the denominator.

(defun two-digit-ratio (n1 n2 d1 d2)
  "generate a ratio from the two top digits and the two bottom digits"
  (/ 
   (+ (* 10 n1) n2)
   (+ (* 10 d1) d2)))


(defun p33 ()
  "yield the denominator of the product of the four desired fractions"
  (denominator  
   (reduce #'* 
	   (mapcar (lambda (f) (apply #'two-digit-ratio f)) 
		   (curious-fractions)))))

(defun curious-fractions ()
  "return the four two digit curious fractions between zero and one"
  (let (result)
    (loop for n1 from 1 to 9 do
	 (loop for n2 from 1 to 9 do
	      (loop for d1 from 1 to 9 do
		   (loop for d2 from 1 to 9 do
			(let ((rat (two-digit-ratio n1 n2 d1 d2)))
			  (cond 
			    ((>= rat 1)) ; do nothing
			    ((= n1 d2) 
			     (when (= rat (/ n2 d1))
			       (push (list n1 n2 d1 d2) result)))
			    ;; ((= n1 d1) ;; i think this case is impossible
			    ;;  (when (= rat (/ n2 d2))
			    ;;    (push (list n1 n2 d1 d2) result)))
			    ;; ((= n2 d2) ;; this case is impossible
			    ;;  (when (= rat (/ n1 d1))
			    ;;    (push (list n1 n2 d1 d2) result)))
			    ((= n2 d1)
			     (when (= rat (/ n1 d2))
			       (push (list n1 n2 d1 d2) result)))))))))
    result))
			   


