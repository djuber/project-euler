;; If we list all the natural numbers below 10 that are multiples of 3 or 5,
;; we get 3, 5, 6 and 9. The sum of these multiples is 23.

;; Find the sum of all the multiples of 3 or 5 below 1000.

(defun p1 (end)
  (let ((s 0))
    (do              ; loop frightens me, but see p1-alt.lisp for examples
     ((x 3 (+ x 1))) ; start at 3
     ((eql x end)) ; end at 1000
      (if
       (or (zerop (mod x 3))
	   (zerop (mod x 5)))
       (incf s x)))
    s))


;;; check validity against sample data
;; CL-USER> (p1 10)
;; 23

;;; run test
;; CL-USER> (p1 1000)
;; 233168