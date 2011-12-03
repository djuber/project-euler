(defun factorial (n &optional (m 1))
  (if (= n 1)
      m
      (factorial (1- n) (* n m))))


(defun digits (n &optional (lst ()))
  "break an integer into a list of its digits"
  (if (zerop n)
      lst
      (digits (floor (/ n 10)) (cons (mod n 10) lst))))


(defun p20 ()
  (apply '+ (digits (factorial 100))))
