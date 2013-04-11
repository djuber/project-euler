
;; Roger Corman's Sieve function from Corman Lisp examples
;; this causes a heap exhaustion for large enough n
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

;; let's save the easy ones, less than 10M
(defparameter *small-primes* (sieve5 (expt 10 7)))
;; and let's not count these over and over again
(defparameter *size-of-small-primes* (length *small-primes*))

;; now we can include an index function
(defun prime-at (n)
  "return the nth prime number"
  (when 
      (and 
       (< n *size-of-small-primes*)
       (integerp n)
       (plusp n))
    (elt *small-primes* (1- n))))

(defun prime-index (p)
  (1+ (position p *small-primes*)))

(defun next-prime (p)
  (let ((idx (prime-index p)))
    (when idx
      (prime-at (1+ idx)))))

;; alternate version:
(defun next-prime (p)
  (let ((list (member p *small-primes*)))
    (when list
      (second list))))

(defun prime-after (n)
  (car (remove-if (lambda (x) (< x n)) *small-primes*)))

(defun prime-before (n)
  (car (last (remove-if (lambda (x) (> x n)) *small-primes*))))


(defun previous-prime (p)
  (let ((idx (prime-index p)))
    (when idx
      (prime-at (1- idx)))))

(defun forward-prime-gap (p)
  (let ((idx (prime-index p)))
    (when idx
      (- (next-prime p) p))))
(defun reverse-prime-gap (p)
  (let ((idx (prime-index p)))
    (when idx
      (- p (previous-prime p)))))

(defun prime-gap (n)
  (- (prime-at (1+ n)) (prime-at n)))

(defun digits (n &key (list ()) (base 10))
  "break an integer into a list of its digits"
  (if (zerop n)
      list
      (digits (floor (/ n base)) :list (cons (mod n base) list) :base base)))

  
(defun palindrome-list (lst)
  "are all elements the same when reversed?"
  (if (null (cdr lst))
      T
      (if (equal (the fixnum (first lst)) (the fixnum (car (last lst))))
	  (palindrome-list (subseq lst 1 (- (length lst) 1)))
	  nil)))
     
(defun palindrome (n)
  (if (palindrome-list (digits n))
      T
      nil))

(defun triangle (n)
  "the nth triangle number"
  (/ (* n (+ n 1)) 2))

(defun square (n)
  "the nth square number"
  (* n n))
 
(defun pentagonal (n)
  "The nth pentagonal number"
  (/ (* n (1- (* 3 n))) 2))
 
(defun hexagonal (n)
  "the nth hexagonal number"
  (* n (1- (* 2 n))))


;; what a horrible thing to do!
(defun triangle-p (n)
  (do* ((index 1 (1+ index))
       (triangle-index (triangle index) (triangle index)))
       ((> triangle-index n))
    (if (= triangle-index n)
	(return-from triangle-p index)))
  nil)

;; and yet you never learn...
(defun pentagon-p (n)
  (do* ((index 1 (1+ index))
	(pentagon-index (pentagonal index) (pentagonal index)))
       ((> pentagon-index n))
    (if (= pentagon-index n)
	(return-from pentagon-p index)))
  nil)

;; and to just keep on doing it? Shame on you!
(defun hexagon-p (n)
  (do* ((index 1 (1+ index))
	(hexagon-index (hexagonal index) (hexagonal index)))
       ((> hexagon-index n))
    (if (= hexagon-index n)
	(return-from hexagon-p index)))
  nil)


(defun lychrel (n &optional (counter 0))
  (if (> counter 50)
      T
      (let ((next (next-candidate n)))
	(if (palindrome next)
	    nil
	    (lychrel next (1+ counter))))))

(defun next-candidate (n)
  (+ n (digits-to-number (reverse (digits n)))))


(defun list-divisors (n)
  (loop for i from 1 to (sqrt n)
     when (= (mod n i) 0)
     collect i
     and unless (= i (/ n i))
     collect (/ n i)))

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


(defun divisors (n)
  (declare (type fixnum n) (optimize (speed 3) (safety 0)))
  (let ((result 0))
    (declare (type fixnum result))
    (do ((div 1 (1+ div)))
	((> div n))
      (if (= 0 (mod n div))
	  (incf result)))
    (the fixnum result)))


(defun collatz-length (n &optional (l 0))
  (cond ((= 1 n) l)
	((evenp n) (collatz-length (/ n 2) (1+ l)))
	((oddp n) (collatz-length (+ 1 (* 3 n)) (1+ l)))))

(defun factorial (n &optional (m 1))
  (if (= n 1)
      m
      (factorial (1- n) (* n m))))

(defun proper-factor (n)
  "return a list of numbers less than n that divide n "
  (if (< n 1)
      ()
      (let ((result '()))
	(do ((test 1 (1+ test)))
	    ((= test n))
	  (if (zerop (mod n test))
	      (setf result (cons test result))))
	result)))

(defun amicable (n)
  (let ((pair (apply '+ (proper-factor n))))
    (and
     (= (apply '+ (proper-factor pair)) n)
     (not (= n pair)))))

(defun split-strings (line)
  (let ((lst ())
	(first 0))
    (do ((index 0 (1+ index)))
	((= index (length line)))
	(if (= (base26-value (char line index)) 0)
	    (progn 
	      (setf lst (cons (subseq line first index) lst))
	      (setf first (1+ index)))))
    (setf lst (cons (subseq line first) lst))
    lst))


(defun alphabetize (line)
  (sort (split-strings line) 'string<))

	
(defun between (n lower upper)
  "is n in interval [lower,upper]"
  (and (>= n lower)
       (<= n upper)))


(defun alphap (a)
  "is a a letter?"
  (or (between (char-code a) (char-code #\A) (char-code #\Z))
      (between (char-code a) (char-code #\a) (char-code #\z))))


(defun upperp (a)
  "is a uppercase?"
  (between (char-code a) (char-code #\A) (char-code #\Z)))

;; didn't need lowercase support here, but it's the right thing to do
(defun base26-value (a)
  (if (not (alphap a))
      0
	(if (upperp a)
	    (- (char-code a) 64)
	    (- (char-code a) 96))))

(defun abundant-p (x)
  (> (reduce #'+ (find-divisors x)) x))


(defun find-divisors (x)
  (let ((divs nil))
    (loop for i from 1 to (sqrt x) 
       do (when (zerop (mod x i)) 
	    (push i divs)
	    (let ((d (/ x i)))
	      (unless (or (= i 1) (= i d))
		(push d divs)))))
    divs))

(defun abundant-sum-odd? (n odds evens)
  (loop for num1 in odds
        until (> num1 n)
        do (loop for num2 in evens
           until (> (+ num1 num2) n)
           do (if (= (+ num1 num2) n)
                  (return-from abundant-sum-odd? t))))
  nil)

 
(defun get-even-abundant-numbers (n)
  (loop for i from 12 to n by 2
        when (abundant-number? i)
        collect i))
 
(defun get-odd-abundant-numbers (n)
  (loop for i from 945 to n by 2
        when (abundant-number? i)
        collect i))
 
(defun abundant-number? (n)
  (> (sum-of-divisors n) n))

(defun sum-of-divisors (n)
  (let ((s 0))
    (do ((test 1 (1+ test)))
	((= test n))
      (if (zerop (mod n test))
	  (incf s test)))
    s))


(defun fib-until-digits (digits)
  (let ((current 1)
	(last 1)
	(count 2)
	(limit (expt 10 (1- digits))))
    (do ((n (+ current last) (+ current last)))
	((> n limit))
      (setf last current)
      (incf count)
      (setf current n))
    (1+ count)))

(defun circular-prime-p (n)
  "Return T if N is a circular prime (all digit rotations are prime.
Note that N is assumed prime."
  (let* ((digits (digit-list n))
	 (num-digits (length digits)))
    (do* ((count 1 (1+ count))
	  (rot (rotate-list digits) (rotate-list rot))
	  (circularp t (primep (digit-list->integer rot))))
	 ((or (not circularp)
	      (> count num-digits))
	  circularp))))


(defun digit-list->integer (list)
"Convert base 10 digit LIST into an integer."
(do* ((digits (reverse list))
      (power 0 (1+ power))
      (dig-list digits (cdr dig-list))
      (value (first dig-list) (if dig-list
				  (+ value
				     (* (first dig-list)
					(expt 10 power)))
				  value)))
     ((null dig-list) value)))


(defun rotate-list (list &optional (n 1))
  "Return LIST rotated left by N."
  (do ((count 0 (1+ count))
       (rot list (append (cdr rot)
			 (list (first rot)))))
      ((>= count n) rot)))



(defun digit-list (n)
  "Return a list of the digits in integer N."
  (loop for c across (format nil "~A" n)
     collect (char-to-val c)))



(defun erat (n)
  "Return a list of prime numbers less than N using the Sieve of Eratosthenes."
  (let ((primes-list (list))  
      ;; The zeroth and first elements of primep table are ignored.
	(primep (make-array n :initial-element t))
	(prime-idx 2))
    (loop while (< prime-idx n) do
	 (let ((step prime-idx)
	       (search-start-idx (* prime-idx prime-idx)))
	   ;; Positioned on a prime, so add it to the list.
	   (setf primes-list (cons prime-idx primes-list))
	   ;; Mark multiples of found prime as not prime.
	   (loop for search-idx from search-start-idx by step
	      while (< search-idx n) do
		(setf (aref primep search-idx) nil))
	   ;; Position prime pointer onto next prime
	   (incf prime-idx)
	   (loop while (< prime-idx n)
	      while (not (aref primep prime-idx)) do
		(incf prime-idx))))
    (nreverse primes-list)))

(defun primep (n)
  (and (integerp n)
       (> n 1)
       (not (cdr (factor n)))))
#|
(defun primep (n)
  "Return T if N is prime."
  (cond ((not (integerp n)) nil)
	((integerp (/ n 2)) (if (= n 2) t nil))
	((integerp (/ n 3)) (if (= n 3) t nil))
	((integerp (/ n 5)) (if (= n 5) t nil))
	((position n (erat 100)) t)
	(t
	 (let ((limit (isqrt n))
	       (divisor-found nil))
	   (loop for a from 31 to (+ limit 31) by 30
	      for b from 7 to (+ limit 7) by 30
	      for c from 11 to (+ limit 11) by 30
	      for d from 13 to (+ limit 13) by 30
	      for e from 17 to (+ limit 17) by 30
	      for f from 19 to (+ limit 19) by 30
	      for g from 23 to (+ limit 23) by 30
	      for h from 29 to (+ limit 29) by 30
	      until divisor-found
	      finally (return (not divisor-found))
	      when (or (integerp (/ n a))
		       (integerp (/ n b))
		       (integerp (/ n c))
		       (integerp (/ n d))
		       (integerp (/ n e))
		       (integerp (/ n f))
		       (integerp (/ n g))
		       (integerp (/ n h)))
	      do (setf divisor-found t))))))

|#

(defparameter char-digits
  '((#\0 0)
    (#\1 1)
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)
    (#\9 9)))



(defun char-to-val (char)
  (second (assoc char char-digits)))

(defun cycle (n)
  "makes a list of all the digits of n rotated all possible ways"
  (let ((lst ())
	(dn (digits n)))
    (do ((i 0 (1+ i)))
	((= i (length dn)))
      (push (digits-to-number (rotate i dn)) lst))
    lst))

;; a single rotation of a list
(defun rotate (n lst)
  "returns a list with the first n elements at the rear of the list"
  (if (= 0 n)
      lst
      (rotate (1- n) (append (rest lst) (list (first lst))))))
;; this cries out for (butlast lst)


;; this reverse (digits n),
;; so (= n (digits-to-number (digits n))) -> T
(defun digits-to-number (lst)
  "undo digits transformation"
  (if (null lst)
      0
      (+ (car (last lst))
	 (* 10 (digits-to-number (reverse (rest (reverse lst))))))))


(defun factor (n)
  "return a list of factors of n"
  (let ((test 3)
	(limit (+ 1 (floor (sqrt n)))))
    (cond
      ((eq n 1) nil)
      ((evenp n) (cons 2 (factor (/ n 2))))
      (t (do
	  ()
	  ((or (zerop (mod n test)) (> test limit)))
	   (incf test 2))
	 (if (> test limit)
	     (list n)
	     (cons test  (factor (/ n test))))))))

(defun string-to-number (s)
  "grabs all digits in string, skipping whitespace and alpha"
  (let ((l (length s))
	(result 0))
    (do ((count 0 (1+ count)))
	((= count l))
      (if (and (> (char-code (elt s count)) 47)
	       (< (char-code (elt s count)) 58))
	  (setf result (+ (- (char-code (elt s count)) 48)
			  (* result 10)))))
    result))

(defun square (x) (* x x))

(defun sum-of-squares (lst)
  (reduce (lambda (x y) (+ x y)) (mapcar 'square lst)))

(defun square-of-sum (lst)
  (square (reduce (lambda (x y) (+ x y)) lst)))


(defun nums (start stop)
  "list of numbers from start to stop inclusive"
  (if (> start stop)
      ()
      (cons start (nums (1+ start) stop))))

(defun naturals-up-to (n)
  (declare (fixnum n))
  (if (zerop n)
      nil
      (cons n (naturals-up-to (1- n)))))


;; this seems redundant given (every pred list)
(defun forall (list func)
  "every element in list is func?"
  (if (null list)
      T
      (and (funcall func (car list))
	   (forall (cdr list) func))))


(defun triple (a b c)
  "are a b and c a pythagorean triple?"
  (= (sum-of-squares (list a b)) (* c c)))


(defun binary-digits (n &optional (lst ()))
  "break an integer into a list of its binary digits"
  (if (zerop n)
      lst
      (binary-digits (floor (/ n 2)) (cons (mod n 2) lst))))

(defun prime-difference (n d)
  "print primes below n with difference d"
	   (let ((primes (sieve5 n))
		 ;;(pairs nil)
		 )
	     (do ((i 0 (1+ i)))
		 ((null primes))
	       (let ((num (first primes)))
		 (if (and (not (null (second primes))) 
			       (= d (- (second primes) num)))
		     ;;(push num pairs)
		     (format t "~a ~a~%" num (+ num d)))
		 (pop primes)))))

(defun twin-primes (n)
  "print twin primes below n"
  (prime-difference n 2))

(defun cousin-primes (n)
  "print cousin primes below n"
  (prime-difference n 4))

(defun sexy-primes (n)
  "print sexy primes below n"
  (prime-difference n 6))


(defun numbparts (n &key (all nil))
  "partition function p(n), adapted from R partitions package"
  (when (< n 2) (return-from numbparts 1))
  (let ((p (make-array (1+ n) :element-type 'integer :initial-element 0))
	(pp (make-array (1+ n) :element-type 'integer :initial-element 0)))
    (labels 
	((outerloop (i)
	   (let ((s 1) ;; sign
		 (f 5) ;; first difference
		 (r 2)) 
	     ;; while loop 1
	     (loop until (< i r)
		do (progn
		     (incf (aref pp i) (* s (aref pp (- i r))))
		     (incf r f)
		     (incf f 3)
		     (setf s (* s -1))))
	     (setf s 1)
	     (setf f 4)
	     (setf r 1)
	     ;; while loop 2
	     (loop until (< i r)
		  do (progn
		       (incf (aref pp i) (* s (aref pp (- i r))))
		       (incf r f)
		       (incf f 3)
		       (setf s (* s -1)))))))
      (setf (aref p 0) 1 (aref p 1) 1 (aref pp 0) 1 (aref pp 1) 1)
      (loop for i from 2 to  n
	 do (outerloop i))
      (if all
	  pp
	  (aref pp n)))))
