;; Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

;; 21 22 23 24 25
;; 20  7  8  9 10
;; 19  6  1  2 11
;; 18  5  4  3 12
;; 17 16 15 14 13

;; It can be verified that the sum of the numbers on the diagonals is 101.

;; What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?


;; for n=1, sum is 1
;; for n=3, sum is sum(1) + (1+2, 1+4, 1+6, 1+8)
;; for n=5, sum is sum(3) + (9+4, 9+8, 9+12, 9+16)
;; for n=7, sum is sum(5) + (25+6, 25+12, 25+18, 25+24)

;; in general sum(n), n odd depends on the last
;; term plus (n-1) as a factor
(defun p28 ()
  (spiral-sum 1001))


(defun spiral-sum (n &optional (last 0))
  (let ((s 1)
	(last 1)) ; if I were smarter, I would have seen last is (n-2)**2
    (do ((rank 3 (+ 2 rank)))
	((> rank n))
    ; (format t "s ~d last ~d ~%" s last)
      (incf s (+ (* 4 last) (* (- rank 1) 10)))
      (incf last (* 4 (- rank 1))))
    s))

