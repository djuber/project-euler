#|
Euler published the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

Using computers, the incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

    n² + an + b, where |a| < 1000 and |b| < 1000

    where |n| is the modulus/absolute value of n
    e.g. |11| = 11 and |−4| = 4

Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.
Euler published the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

Using computers, the incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

    n² + an + b, where |a| < 1000 and |b| < 1000

    where |n| is the modulus/absolute value of n
    e.g. |11| = 11 and |−4| = 4

Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.
|#

;; primep defined in util.lisp

(defun p27-test ()
  (let ((a 1)
	(b 41)
	(highest 1)
	(product 1))
    (do* 
     ((n 0 (1+ n))
      (sum  
       (+ (* n n) (* a n) b)  
       (+ (* n n) (* a n) b)))
     ((not (and 
	    (plusp sum)
	    (primep sum)))
      (if (> n highest)
	  (progn 
	    (setf highest (1- n))
	    (setf product (* a b)))
	  nil))
      (format t "n: ~A sum:~A a:~A b:~A~%"
	      n sum a b))
    (values highest product)))


(defun p27 ()
  (let ((highest 1)
	(product 1))
    (do ((a (- 1000) (1+ a)))
	((> a 1000) nil)
      ;;(format t "A is now ~A~%" a)
      (do ((b 1 (1+ b)))
	  ((> b 1000) nil)
	;;(format t "B is now ~A ~%" b)
	(do* 
	 ((n 0 (1+ n))
	  (sum  
	   (+ (* n n) (* a n) b)  
	   (+ (* n n) (* a n) b)))
	 ((not (and 
		(plusp sum)
		(primep sum)))
	  (if (> n highest)
	      (progn 
		(setf highest (1- n))
		(setf product (* a b)))
	      nil)))))
	  ;;(format t "n: ~A sum:~A a:~A b:~A~%"
	  ;; n sum a b))))
    (values product highest)))

(p27)

#|
CL-USER> (time (p27))
Evaluation took:
  2.918 seconds of real time
  2.980186 seconds of total run time (2.980186 user, 0.000000 system)
  [ Run times consist of 0.477 seconds GC time, and 2.504 seconds non-GC time. ]
  102.12% CPU
  8,535,222,626 processor cycles
  1,022,592,432 bytes consed
  
-59231
70
|#
