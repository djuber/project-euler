#|
27 Aug 2007 11:35 am 
wtwood
LISP 
USA	


CMU Common Lisp, compiled. I didn't see the combinatorial analysis and got interested in the problem as a problem in discrete dynamics. I implemented a map from set of integers to set of integers that carried along the number of integers that map to an integer by the digit-squaree-sum function. It was interesting to see how the iterated images contracted. I'd guess that a proof of convergence can be based on that. It takes ca. 17 seconds.
	
|#

(defun euler098 ()
  (cadr (attractors-of-digit-squares-sum)))

(defun attractors-of-digit-squares-sum ()
  (iterate loop ((image (image-of-9999999)))
    (if (= (hash-table-count image) 2)
        (list (gethash 1 image) (gethash 89 image))
        (loop (iterate-dss image)))))

(defun iterate-dss (image)
  (let ((ht (make-hash-table :size 600)))
    (maphash #'(lambda (k v)
                 (unless (or (= k 89) (= k 1))
                   (incf (gethash (digit-squares-sum k) ht 0) v)))
             image)
    (incf (gethash 1 ht 0) (gethash 1 image))
    (incf (gethash 89 ht 0) (gethash 89 image))
    ht))

(defun image-of-9999999 ()
  (let ((ht (make-hash-table :size 600)))
    (do ((i 1 (1+ i)))
        ((> i 9999999) ht)
      (incf (gethash (digit-squares-sum i) ht 0)))))
