
;; The sum of the squares of the first ten natural numbers is,
;; 1^(2) + 2^(2) + ... + 10^(2) = 385

;; The square of the sum of the first ten natural numbers is,
;; (1 + 2 + ... + 10)^(2) = 55^(2) = 3025

;; Hence the difference between the sum of the squares of the first ten natural
;; numbers and the square of the sum is 3025 − 385 = 2640.

;; Find the difference between the sum of the squares of the first one hundred
;; natural numbers and the square of the sum.

;; Answer:
;; 	25164150

(defun square (x) (* x x))
(defun sum-of-squares (lst)
  (reduce (lambda (x y) (+ x y)) (mapcar 'square lst)))
(defun square-of-sum (lst)
  (square (reduce (lambda (x y) (+ x y)) lst)))
(defun difference (lst)
  (- (square-of-sum lst) (sum-of-squares lst)))
(defun naturals-up-to (n)
  (declare (fixnum n))
  (if (zerop n)
      nil
      (cons n (naturals-up-to (1- n)))))

(defun p6 (n)
  (difference (naturals-up-to n)))
