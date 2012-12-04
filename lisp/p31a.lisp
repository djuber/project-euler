#|
28 Oct 2005 08:26 pm 
JonRock
|#	

(defun fill-partitions-hash (max)
  (setq partitions-hash (make-hash-table :test 'equal))
  (setf (gethash (list 0 0) partitions-hash) 1)
  (setf (gethash (list 1 1) partitions-hash) 1)
  (loop for n from 1 to max
        do (fill-partitions-helper n n)))

(defun fill-partitions-helper (n max)
  (let ((memo (gethash (list n max) partitions-hash)))
    (if memo
        memo
      (loop for i in '(1 2 5 10 20 50 100 200)
            while (<= i max)
            sum (fill-partitions-helper (- n i) (min i (- n i) max)) into p
            finally (return (setf (gethash (list n max) partitions-hash) p))))))

(defun euler31 ()
  (fill-partitions-hash 200)
  (gethash (list 200 200) partitions-hash))
