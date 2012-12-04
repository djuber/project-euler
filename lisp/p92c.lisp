#|
06 Jan 2008 09:32 pm 
tayloj
LISP 
USA	
   Quote    0

I kept a table of the values to which the values from 1..567 eventually converge, and then iterated from 1..9999999 counting when the successor went to 89. Common Lisp:
	
|#

(defun problem92 (&aux (h (make-hash-table :size 657)))
  (labels ((succ (n &aux (s 0) (r 0))
	     (do () ((zerop n) s)
	       (setf (values n r) (floor n 10))
	       (incf s (* r r))))
	   (number-chain (n)
	     (do ((n n (succ n)))
		 ((or (eql n 1) (eql n 89)) n))))
    (do ((i 567 (1- i))) ((zerop i))
      (setf (gethash i h) (number-chain i)))
    (loop for n from 1 below 10000000
       count (eql 89 (gethash (succ n) h)))))

#|
cl-user> (time (problem92))
Timing the evaluation of (problem92)
User time = 9.460
System time = 0.087
Elapsed time = 10.992
|#