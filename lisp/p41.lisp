;; We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.
;; For example, 2143 is a 4-digit pandigital and is also prime.

(let ((i 7654231)) ;; 8 or 9 digit pandigitals are all divisible by 3
	   (do ((k i (1- k)))
	       ((pandigital-prime-p k))
	     (setf i k))
	   i)
;;(7652413)




;; What is the largest n-digit pandigital prime that exists?
(defun digits (n &optional (lst ()))
  "break an integer into a list of its digits"
  (if (zerop n)
      lst
      (digits (floor (/ n 10)) (cons (mod n 10) lst))))

;; useful? test function
;; a number is pandigital if it contains each of the digits up to its
;; length
(defun pandigital-p (n)
  (let ((ndigits (digits n)))
    (let ((m (length ndigits)))
      (let ((all-digits (loop for i from 1 to m collecting i)))
	(every (lambda (x) (member x ndigits)) all-digits)))))

(defun prime (n)
  (member n (sieve5 (+ n 1))))

(defun pandigital-prime-p (n)
  (and (pandigital-p n)
       (prime n)))

;; it seems like a better way is to generate the n n-pandigital numbers for
;; 8 down in descending order, and test for primality

(defun cycle (n)
  "makes a list of all the digits of n rotated all possible ways"
  (let ((lst ())
	(dn (digits n)))
    (do ((i 0 (1+ i)))
	((= i (length dn)))
      (push (digits-to-number (rotate i dn)) lst))
    lst))

;; (rotate n k lst)
(defun rotate2 (n lst &optional k)
  "rotate last k elements of lst n places"
  )

(defun permute (n)
  "makes a list of all the digits of n permuted all possible ways"
  (let ((lst ())
	(dn (sort (digits n) #'>)))
    (do ((i 1 (1+ i)))
	((> i (length dn)))
      (do ((j 1 (1+ j)))
	  ((= j i))
	(push (digits-to-number (append (butlast dn i)
					(rotate (last dn i) j))) lst)
	(push (digits-to-number (append (rotate (butlast dn i) j)
					(last dn i))) lst)))
    lst))

;; this reverse (digits n),
;; so (= n (digits-to-number (digits n))) -> T
(defun digits-to-number (lst)
  "undo digits transformation"
  (if (null lst)
      0
      (+ (car (last lst))
	 (* 10 (digits-to-number (reverse (rest (reverse lst))))))))



;; a single rotation of a list
(defun rotate (n lst)
  "returns a list with the first n elements at the rear of the list"
  (if (= 0 n)
      lst
      (rotate (1- n) (append (rest lst) (list (first lst))))))


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

