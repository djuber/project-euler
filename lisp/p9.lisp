;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;; a^(2) + b^(2) = c^(2)

;; For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).

;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;; Find the product abc.

;; Answer: 31875000
(defun p9 ()
  (let ((answer))
    (do ((a 1 (1+ a)))
	((= a 500))
      (do ((b a (1+ b)))
	  ((= b 500))
	(if (p9-triple a b)
	    (progn
	      (format t "A ~d B ~d C ~d~%" a b (sqrt (sum-of-squares a b)))
	      (setf answer (* a b (- 1000 a b)))))))
    answer))

(defun p9-triple (a b)
  (triple a b (- 1000 a b)))

(defun triple (a b c)
  (= (sum-of-squares a b) (* c c)))

(defun sum-of-squares (a b)
  (+ (* a a) (* b b)))



