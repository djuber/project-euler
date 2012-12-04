(defun p76 ()
  "how many ways can 100 be written as the sum of positive integers?"
  (1- (numbparts 100)))


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