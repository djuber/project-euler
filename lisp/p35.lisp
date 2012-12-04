;; The number, 197, is called a circular prime because all rotations
;; of the digits: 197, 971, and 719, are themselves prime.

;; There are thirteen such primes below 100:
;; 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

;; How many circular primes are there below one million?

;; answer is 55 

;; again we start with a prime sieve
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

;; digits will make a list of digits
(defun digits (n &optional (lst ()))
  "break an integer into a list of its digits"
  (if (zerop n)
      lst
      (digits (floor (/ n 10)) (cons (mod n 10) lst))))

;; this reverse digits
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
;; this cries out for (butlast lst)

;; all the rotations of a number
(defun cycle (n)
  "makes a list of all the digits of n rotated all possible ways"
  (let ((lst ())
	(dn (digits n)))
    (do ((i 0 (1+ i)))
	((= i (length dn)))
      (push (digits-to-number (rotate i dn)) lst))
    lst))

;; wrap the sieve result around the function to only call at compile time
(let ((p (sieve5 2000000)))
  ;; it's important the way this is code
  
  (defun clean-p ()
    (do ((i 0 (1+ i)))
	((= i (length p)))
      (let ((prime-digits (digits (nth i p))))
	(if (and (> (length prime-digits) 1)
		 (or (member 2 prime-digits)
		     (member 4 prime-digits)
		     (member 5 prime-digits)
		     (member 0 prime-digits)
		     (member 6 prime-digits)
		     (member 8 prime-digits)))
	    (progn (setf p (remove (nth i p) p))
		   (decf i))))))

  ;; a quick diagnostic (to see if clean-p worked
  (defun print-p ()
    p)

  (defun circular-prime-p (n)
    "is n a circular prime"
    (let ((c (cycle n))
	  (result t))
      (let ((dc (digits (first c))))
	(if (and (> (length dc) 1)
		 (or (member 2 dc)
		     (member 5 dc)))
	    (setf result nil)
	    (do ((i 0 (1+ i)))
		((= i (length c)))
	      (if (not (member (nth i c) p))
		  (setf result nil)))))
      result))
  ;; should be 13 for n = 100
  (defun p35 (n)
    "count circular primes to n"
    (let ((return 0))
      (do* ((i 0 (1+ i))
	    (m (nth i p) (nth i p)))
	   ((> m n))
	(if (circular-prime-p m)
	    (incf return)))
      return)))
    
