;; Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

;;     1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
;;     8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
;;     9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)

;; As 1 = 1^(4) is not a sum it is not included.

;; The sum of these numbers is 1634 + 8208 + 9474 = 19316.

;; Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

(defun p30 (limit)
  (let ((s 0))
    (do ((n 2 (1+ n)))
	((> n limit)) ;magic number is 9**5 * 5 
      (if (= n (sum-of-fifth-powers (digits n)))
	  (incf s n)))
    s))
	     
(defun sum-of-fifth-powers (lst)
  (if (null lst)
      0
      (+ (expt (car lst) 5) (sum-of-fifth-powers (rest lst)))))


(defun digits (n &optional (lst ()))
  "break an integer into a list of its digits"
  (if (zerop n)
      lst
      (digits (floor (/ n 10)) (cons (mod n 10) lst))))
