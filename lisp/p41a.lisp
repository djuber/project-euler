(defun euler41 (&optional (limit 987654321))
  (let ((pandigitals (permute-num limit))
        (primes-found (reverse (list-primes (isqrt limit)))))
    (let ((x (car (remove-if
                   #'(lambda (x)
                       (reduce
                        #'(lambda (a b)
                            (or a (zerop (mod x b))))
                        primes-found
                        :initial-value nil))
                   pandigitals))))
      (if x x (euler41 (mod limit
                            (expt 10 (floor (log limit 10)))))))))
 
(defun list-primes (&optional (sieve 1000000))
  (let ((numbers (make-array sieve :initial-element t))
        (upper-bound (isqrt (- sieve 1)))
        (lst '()))
    (do ((i 2 (1+ i))) ((> i upper-bound))
      (when (svref numbers i)
        (push i lst)
        (do ((j (* i i) (+ j i))) ((> j (1- sieve)))
          (setf (svref numbers j) nil))))
    (do ((i (1+ upper-bound) (1+ i))) ((= i sieve) lst)
      (when (svref numbers i)
        (push i lst)))))
 
(defun permute-num (num)
  (let ((lst (permute (num->digits num))))
    (remove-duplicates (mapcar #'digits->num lst))))
 
(defun digits->num (lst)
  (reduce #'(lambda (a b) (+ (* 10 a) b)) lst))
 
(defun num->digits (num)
  (map 'list #'(lambda (char)
                 (read-from-string (string char)))
       (prin1-to-string num)))
 
(defun permute (list)
  (if (null list)
      (list nil)
      (mapcan #'(lambda (first) 
                  (mapcar #'(lambda (rest)
                              (cons first rest))
                          (permute (remove first list :count 1 :test #'eq))))
              list)))
 
(defun pandigital-p (n)
  (let ((p (search (sort (prin1-to-string n) #'char<) "123456789")))
    (if (and (integerp p) (zerop p)) t nil)))