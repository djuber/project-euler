;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6^(th) prime is 13.

;; What is the 10001^(st) prime number?

;; Answer:
;; 	104743
(defun forall (list func)
  "every element in list is func?"
  (if (null list)
      T
      (and (funcall func (car list))
	   (forall (cdr list) func))))

(defun nums (start stop)
  "list of numbers from start to stop inclusive"
  (if (> start stop)
      ()
      (cons start (nums (1+ start) stop))))

(defun prime (n)
  "lots of trial divisions ahead. Not optimized"
  (and
   (> n 1)
   (forall (nums 2 (floor (sqrt n))) 
	   (lambda (divisor) (not (= (mod n divisor) 0))))))
	  
(defun nth-prime (n)
  "returns the nth prime"
  (let ((candidate 2))
    (do ((nth 1))
	((= nth n))
      (incf candidate) ; checks all the evens, at least prime fails those fast
      (if (prime candidate)
	  (incf nth)))
    candidate))
    
(defun p7 ()
  (nth-prime 10001))


