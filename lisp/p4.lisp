;; Problem 4
;; 16 November 2001

;; A palindromic number reads the same both ways. The largest palindrome
;; made from the product of two 2-digit numbers is 9009 = 91 × 99.

;; Find the largest palindrome made from the product of two 3-digit numbers.

;; Answer:
;; 	906609


;; the wrong thing to do is start small and work your way up

(defun p4 ()
  (let ((m 0))
    (do ((i 100 (1+ i)))
	((eql i 1000))
      (do ((j 100 (1+ j)))
	  ((eql j 1000))
	(if (palindrome (* i j))
	    (if (> (* i j) m)
		(setf m (* i j))))))
    m))



;; the right thing to do is start at the top and descend
;; until you find a palindrome (this is trickier)

(defun digits (n &optional (lst ()))
  "break an integer into a list of its digits"
  (if (zerop n)
      lst
      (digits (floor (/ n 10)) (cons (mod n 10) lst))))
  
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

;; CL-USER> (time (p4))
;; Evaluation took:
;;   2.560 seconds of real time
;;   2.400000 seconds of total run time (2.370000 user, 0.030000 system)
;;   [ Run times consist of 0.030 seconds GC time, and 2.370 seconds non-GC time. ]
;;   93.75% CPU
;;   5,962,935,439 processor cycles
;;   346,867,312 bytes consed
  
;; 906609
;; CL-USER> 


;; not much faster : optimizing here isn't where the work is
;; better to find a faster implementation of palindrome/palindrome-list/digits

(defun p4a ()
  (proclaim '(optimize (speed 3) (safety 0)))
  (let ((m 0))
    (declare (type fixnum m))
    (do ((i 999 (1- i)))
	((eql i 100))
      (declare (type fixnum i))
      (do ((j 999 (1- j)))
	  ((eql j 100))
	(declare (type fixnum j))
	(if (palindrome (the fixnum (* i j)))
	    (if (> (the fixnum (* i j)) m)
		(setf m (* i j))))))
    m))
