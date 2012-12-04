#|
Problem 92
01 April 2005

A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.

For example,

44  32 -- 13 -- 10 -- 1 -- 1
85 -- 89 -- 145 -- 42 -- 20 -- 4 -- 16 -- 37 -- 58 -- 89

Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.

How many starting numbers below ten million will arrive at 89?
|#

(defun digits (n &optional (lst ()))
  "break an integer into a list of its digits"
  (if (zerop n)
      lst
      (digits (floor (/ n 10)) (cons (mod n 10) lst))))

(defun sum-of-square-of-digits (n)
  (let ((digits (digits n)))
    (reduce #'+ (mapcar  (lambda (x) (* x x))
			 digits))))

(defun eighty-nine-p (n)
  (cond ((= n 89) t)
	((= n 1) nil)
	(t (eighty-nine-p (sum-of-square-of-digits n)))))

(defun p92 ()
  (let ((count 0))
    (loop for i from 1 upto 9999999
	  when (eighty-nine-p i) do (incf count))
    count))

#|

clearly this is not optimal, but meets the one minute criterion

CL-USER> (time (p92))
Evaluation took:
  57.880 seconds of real time
  58.139634 seconds of total run time (58.139634 user, 0.000000 system)
  [ Run times consist of 12.988 seconds GC time, and 45.152 seconds non-GC time. ]
  100.45% CPU
  169,357,209,618 processor cycles
  23,299,104,256 bytes consed
|#