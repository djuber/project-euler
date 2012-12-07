#|
28 Sep 2007 03:42 pm 
wtwood
LISP 
USA	

CMU Common Lisp, compiled. I borrowed some Scheme code for a continued fraction package I wrote. I found both Rosen and Giblin to be useful references. Not too bad on speed -- ca. 86 milliseconds for d <= 1000 and ca. 8.6 seconds for d <= 10000.

|#	


(defun euler066 ()
  (labels ((perfect-squarep (n)
             (let ((s (isqrt n))) (= n (* s s)))))
    (let ((maxx 0)
          (maxx-d nil))
      (do ((i 1 (1+ i)))
          ((> i 10000) (list maxx-d maxx))
        (unless (perfect-squarep i)
          (let ((x (car (minimum-pell-solution i))))
            (if (> x maxx)
                (setf maxx-d i maxx x))))))))

(defun minimum-pell-solution (d)
  (destructuring-bind (p as) (square-root-pscf d)
    (if (evenp p)
        (let ((p/q (periodic-pscf-convergent as (1- p))))
          (list (numerator p/q) (denominator p/q)))
        (let ((p/q (periodic-pscf-convergent as (1- (* 2 p)))))
          (list (numerator p/q) (denominator p/q))))))

(defun square-root-pscf (n)
  (let* ((sqrtN (isqrt n))
         (a0 sqrtN)
         (a0*2 (* 2 sqrtN))
         (as (list a0))
         (as-rear as))
    (iterate loop ((a a0)
                   (capP 0)
                   (capQ 1)
                   (k 0))
      (if (= a a0*2)
          (progn (setf (cdr as-rear) (cdr as)) (list k as))
          (let* ((capP (- (* a capQ) capP))
                 (capQ (/ (- n (* capP capP)) capQ))
                 (x (/ (+ capP sqrtN) capQ))
                 (a (floor x))
                 (a-entry (list a)))
            (setf (cdr as-rear) a-entry)
            (setf as-rear a-entry)
            (loop a capP capQ (1+ k)))))))

(defun periodic-pscf-convergent (pscf n)
  (iterate loop ((pk-1 1)
                 (qk-1 0)
                 (pk (car pscf))
                 (qk 1)
                 (as (cdr pscf))
                 (convergents '())
                 (k 0))
    (if (> k n)
        (car convergents)
        (let ((ak+1 (car as)) (rest-as (cdr as)))
          (loop pk
                qk
                (+ (* ak+1 pk) pk-1)
                (+ (* ak+1 qk) qk-1)
                rest-as
                (cons (/ pk qk) convergents)
                (1+ k))))))

;; The function (square-root-pscf N) returns the period and a circular list representation of the terms of the continued fraction for N. The function (periodic-pscf-convergent PCSF N) returns the N-th convergent of the terms AS of a periodic continued fraction. The function (minimum-pell-solution D) returns the minimum x solution of the Pell equation for D. The function (euler066) runs through the 100 values for d and returns the maximum x and the d at which it occurred.
