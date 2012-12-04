;; Let d(n) be defined as the sum of proper divisors of n
;; (numbers less than n which divide evenly into n).
;; If d(a) = b and d(b) = a, where a ≠ b, then a and b are an
;; amicable pair and each of a and b are called amicable numbers.

;; For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110;
;; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

;; Evaluate the sum of all the amicable numbers under 10000.
(defun p21 (&optional (limit 10000))
  (let ((s 0))
    (do ((a 1 (1+ a)))
	((= a limit))
      (if (amicable a)
	  (incf s a)))
    s))


(defun proper-factor (n)
  "return a list of numbers less than n that divide n "
  (if (< n 1)
      ()
      (let ((result '()))
	(do ((test 1 (1+ test)))
	    ((= test n))
	  (if (zerop (mod n test))
	      (setf result (cons test result))))
	result)))

;; first time round forgot to check for pair = n
(defun amicable (n)
  (let ((pair (apply '+ (proper-factor n))))
    (and
     (= (apply '+ (proper-factor pair)) n)
     (not (= n pair)))))
