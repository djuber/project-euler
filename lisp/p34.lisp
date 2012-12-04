;; Problem 34
;; 03 January 2003

;; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

;; Find the sum of all numbers which are equal to the sum of the factorial of their digits.

;; Note: as 1! = 1 and 2! = 2 are not sums they are not included.

;; note 9! * 7 = 2540160, so no number larger than this can possibly be the sum of the factorial of its digits
;; this is likely a huge overestimation

(defun p34 (&optional (limit 2540160))
  "sum all numbers which are equalt to the sum of the factorial of their digits"
  (let ((s 0))
    (do ((i 10 (1+ i)))
	((= i limit))
      (if (= i (sum-of-fact-of-digits i))
	  (progn
	    (format t "~a~%" i)
	    (incf s i))))
    s))

(defun sum-of-fact-of-digits (n)
  (reduce (lambda (x y) (+ x y)) (mapcar 'factorial (digits n))))


(defun factorial (n &optional (m 1))
  (if (< n 2)
      m
      (factorial (1- n) (* n m))))

(defun digits (n &optional (lst ()))
  "break an integer into a list of its digits"
  (if (zerop n)
      lst
      (digits (floor (/ n 10)) (cons (mod n 10) lst))))

