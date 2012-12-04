;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

;; Find the sum of all the primes below two million.

;; Answer:
;; 	142913828922

(defun p10 (n)
  (let ((primes (sieve5 n)))
    (apply '+ primes)))
;; this takes about a tenth of a second!

;; Roger Corman's Sieve function from Corman Lisp examples
(defun sieve5 (n)
  "Returns a list of all primes from 2 to n"
  (declare (fixnum n) (optimize (speed 3) (safety 0)))
  (let* ((a (make-array n :element-type 'bit :initial-element 0))
	 (result (list 2))
	 (root (isqrt n)))
    (declare (fixnum root))
    (do ((i 3 (the fixnum (+ i 2))))
	((>= i n) (nreverse result))
      (declare (fixnum i))
      (progn (when (= (sbit a i) 0)
	       (push i result)
	       (if (< i root)
		   (do* ((inc (+ i i))
			 (j (* i i) (the fixnum (+ j inc))))
			((>= j n))
		     (declare (fixnum j inc))
		     (setf (sbit a j) 1)))))))) 

