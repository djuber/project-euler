(defun digits (num)
  (map 'list
       #'(lambda (char) (read-from-string (string char)))
       (prin1-to-string num)))

(defun digit-val (digits)
  (read-from-string (format nil "~{~D~}" digits)))

(defun seq-list (min max)
  (loop for i from min to max collect i))

(defun sieve (lst)
  (let ((primes '())
	(last (car (last lst))))
    (loop while (and lst (> last (* (car lst) (car lst))))
       do (let ((factor (car lst)))
	    (setq primes (cons factor primes))
	    (setq lst (remove-if
		       #'(lambda (n)
			   (= (mod n factor) 0))
		       (cdr lst)))))
    (append (reverse primes) lst)))

(defun all-primes (limit)
  (sieve (seq-list 2 limit)))

(defun is-n-pandigital (num)
  (let* ((dig (digits num))
	 (len (length dig))
	 (match (seq-list 1 len)))
    (equal match (sort dig #'<))))

(defun euler41 ()
  (reduce #'max (remove-if-not #'is-n-pandigital (all-primes 7654321)))) 