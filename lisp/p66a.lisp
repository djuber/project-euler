#|
06 Aug 2007 12:07 am 
msingh
LISP 
Australia	
|#

(defun sol-p (convergent d)
  (let ((x (numerator convergent))
        (y (denominator convergent)))
    (and (= (- (* x x) (* d y y)) 1) x)))
  
(defun euler (n)
  (iter (for d from 2 to n)
        (unless (squarep d)
          (for r = (period-length d))
          (for exp = (expansion d (* 2 r)))
          (for x = (iter @ (for i from 1 to (length exp))
                          (for solp = (sol-p (convergent exp i) d))
                          (if solp (return-from @ solp))))
          (finding d maximizing x))))

;; Takes about half a second. 
;; MSINGH> (euler 1000)
;; 661

