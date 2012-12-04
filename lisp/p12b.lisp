;; less than half a second for this version
;; study its glory, and note the absence of declarations

(defun euler12 (&optional (divisors 500))	
  (do* 
   ((n 1 (1+ n)) 
    (tn 1 (+ n tn)))		
   ((> (num-divisors (prime-factor tn)) divisors) tn)))
       
;; this could also be called count unique elements (list)
(defun num-divisors (lst)
  "given a list of divisors, count them"
  (reduce #'* (mapcar #'(lambda (i)
                          (1+ (count i lst)))
                      (remove-duplicates lst))))
 

(defun prime-factor (n)
  "generate the prime factorization of a number n"
  (when (> n 1)
    (let ((limit (1+ (isqrt n))))
      (do ((i 2 (1+ i))) ((> i limit) (list n))
        (when (zerop (mod n i))
          (return-from prime-factor
            (cons i (prime-factor (/ n i)))))))))
