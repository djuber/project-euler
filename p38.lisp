;; Take the number 192 and multiply it by each of 1, 2, and 3:

;;     192 × 1 = 192
;;     192 × 2 = 384
;;     192 × 3 = 576

;; By concatenating each product we get the 1 to 9 pandigital, 192384576.
;; We will call 192384576 the concatenated product of 192 and (1,2,3)

;; The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital,
;; 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

;; What is the largest 1 to 9 pandigital 9-digit number that can be formed as the
;; concatenated product of an integer with (1,2, ... , n) where n > 1?

;; (defun member (a lat)
;;   (if (null lat)
;;       nil
;;       (or (eq (car lat) a)
;; 	  (member a (rest lat)))))

(defun pandigital-p (n)
  "does n contain the digits 1 through 9 each uniquely?"
  (let ((dl (digits n)))
    (and (= (length dl) 9)
	 (forall '(1 2 3 4 5 6 7 8 9) (lambda (n) (member n dl))))))

;; need to know how many times to make this concatenation
(defun needed-iterations (n)
  (loop for i from 1 to 10
       when (< 123456789 (concat-products n i) 987654321)
       return (concat-products n i)))

(defun concat-products (n limit)
  (let ((products (loop for i from 1 to limit
		     collect (* n i))))
    (let ((p* (format nil "~{~a~}" (map 'list #'prin1-to-string products))))
      (string-to-number p*))))

(defun p38 ()
  (let ((largest 1))
    (do ((n 10000 (1- n)))
	((= n 10) largest) ; let this finish
      (let ((number (needed-iterations n)))
	(when (and 
	       number
	       (pandigital-p number)
	       (> number largest))
	  (setf largest number))))))
