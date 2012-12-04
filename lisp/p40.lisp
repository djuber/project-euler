;; An irrational decimal fraction is created by concatenating the positive integers:

;; 0.123456789101112131415161718192021...

;; It can be seen that the 12^(th) digit of the fractional part is 1.

;; If d_(n) represents the n^(th) digit of the fractional part, find the value of the following expression.

;; d_(1) × d_(10) × d_(100) × d_(1000) × d_(10000) × d_(100000) × d_(1000000)

;; jonrock version
(defun euler40 ()
  (let ((digits (loop for i from 0 to 200000
		   append (digits i))))
    (apply #'*
	   (mapcar #'(lambda (n) (nth n digits))
		   '(1 10 100 1000 10000 100000 1000000)))))


(defun make-naturals-digits-list (digits)
  (let ((lst ())
	(counter 0))
    (do ((n 1 (1+ n)))
	((> counter digits))
      (let ((dl (digits n)))
	(setf lst (nconc lst dl))
	(incf counter (length dl))))
    lst))

(defun digits (n &optional (lst ()))
  "break an integer into a list of its digits"
  (if (zerop n)
      lst
      (digits (floor (/ n 10)) (cons (mod n 10) lst))))


