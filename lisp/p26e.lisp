#|
fax
LISP  	
   Quote    0

I just implemented the long division algorithm, make a list of digits and if you ever do the same long-division-digits call then we've hit a cycle, so note that down with an @
|#	


(defun reciporical (d)
  (let ((calls (make-hash-table :test #'equal)))
    (labels ((long-division-digits (num
                                    divisor
                                    &aux (key (cons num divisor)))
               (multiple-value-bind (n r)
                   (floor num divisor)
                 (when (gethash key calls)
                   (return-from long-division-digits
                     `(@ ,(if (= n 0) (floor (* num 10) divisor) n))))
                 (setf (gethash (cons num divisor) calls) t)
                 (cond ((= n 0)
                        (long-division-digits (* num 10) divisor))
                       ((= r 0)
                        (list n))
                       (t (cons n
                                (long-division-digits r divisor)))))))
      (append
       (loop repeat (floor (log d 10))
             collect 0)
       (long-division-digits 1 d)))))

(defun reciporical-cycle-length (n)
  (let ((digits (reciporical n)))
    (unless (eq (car (last (butlast digits))) '@)
      (return-from reciporical-cycle-length 0))
    (- (length digits)
       2
       (position (car (last digits)) digits))))

(caar
 (sort
  (loop for i from 1 below 1000
        collect (list i (reciporical-cycle-length i)))
  #'>
  :key #'second))