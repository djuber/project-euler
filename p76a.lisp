;; Jon Rock
;; 28 Oct 2005

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
      (loop for i from 1 to max
            sum (fill-partitions-helper (- n i) (min i (- n i) max)) into p
            finally (return (setf (gethash (list n max) partitions-hash) p))))))

(defun euler76 ()
  (fill-partitions-hash 200)
  (gethash (list 100 99) partitions-hash))