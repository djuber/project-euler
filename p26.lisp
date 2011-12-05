;; ;; Problem 26
;; 13 September 2002

;; A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

;;     1/2	= 	0.5
;;     1/3	= 	0.(3)
;;     1/4	= 	0.25
;;     1/5	= 	0.2
;;     1/6	= 	0.1(6)
;;     1/7	= 	0.(142857)
;;     1/8	= 	0.125
;;     1/9	= 	0.(1)
;;     1/10	= 	0.1

;; Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

;; Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.


;; lengths are not right (off by a few) but should be within
;; some reciprocal digit lists are just plain wrong (990 is a great example)
(defun reciprocal (number)
  ;; (floor (/ 10.0 number)) looks promising
  (do ((numer 10.0
              (* 10
                 (- numer
                    (* number
                       (floor
                        (/ numer number))))))
       (digit
        (list 0)
        (append digit
                (list
                 (floor
                  (/ numer number)))))
       (remainders (list 0)
                   (push (mod numer number) remainders)))
      ;; someone should hit this guy with a hammer! 
      ((member
        (mod numer number)
        (cdr remainders))
       (values digit remainders
               (1+
                (-
                 (length remainders)
                 (length
                  (member
                   (mod numer number)
                   (cdr remainders)))))))))


(defun p26 ()
  (let ((max 0)
        (maxi 0))
    (loop for i from 2 to 1000
       do (multiple-value-bind (a b c) (reciprocal i)
            (declare (ignore a b))
            (when (> c max)
              (setf max c)
              (setf maxi i))))
    (values maxi max)))

;; (p26)
;; 983
;; 982
