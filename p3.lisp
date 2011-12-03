;; The prime factors of 13195 are 5, 7, 13 and 29.

;; What is the largest prime factor of the number 600851475143 ?
;; CL-USER> (FACTOR 600851475143)
;; (71 839 1471 6857)

(defun p3 (n)
  (car (last (factor n))))

(defun factor (n)
  "return a list of factors of n"
  (let ((test 3)
	(limit (+ 1 (floor (sqrt n)))))
    (cond
      ((eq n 1) nil)
      ((evenp n) (cons 2 (factor (/ n 2))))
      (t (do
	  ()
	  ((or (zerop (mod n test)) (> test limit)))
	   (incf test 2))
	 (if (> test limit)
	     (list n)
	     (cons test  (factor (/ n test))))))))

  
  
  
