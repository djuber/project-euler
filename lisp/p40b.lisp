;; Peatey   (LISP)  Peatey is from USA

(defun euler40 (&optional (digits '(1 10 100 1000 10000 100000 1000000)))
  (apply '* (mapcar 'nth-digit digits)))
 
(defun nth-digit (n &optional (d 1))
  "returns n-th digit (from 1) of fractional part"
  (let ((td (* 9/10 d (expt 10 d))))
    (if (<= n td)
        (ith-digit (+ (floor (1- n) d) (expt 10 (1- d))) (mod (1- n) d))
        (nth-digit (- n td) (1+ d)))))
 
(defun ith-digit (n i)
  "returns i-th digit (from 0) of integer n"
  (- (char-int (char (format nil "~A" n) i)) (char-int #\0)))