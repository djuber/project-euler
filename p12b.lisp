;; less than half a second for this version
;; study its glory, and not the absence of declarations

(defun euler12 (&optional (divisors 500))	
  (do* ((n 1 (1+ n)) (tn 1 (+ n tn)))		
       ((> (num-divisors (prime-factor tn)) divisors) tn)))
       
(defun num-divisors (lst)
  (reduce #'* (mapcar #'(lambda (i)
                          (1+ (count i lst)))
                      (remove-duplicates lst))))
 
(defun prime-factor (n)
  (when (> n 1)
    (let ((limit (1+ (isqrt n))))
      (do ((i 2 (1+ i))) ((> i limit) (list n))
        (when (zerop (mod n i))
          (return-from prime-factor
            (cons i (prime-factor (/ n i)))))))))
