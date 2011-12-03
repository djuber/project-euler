;; JonRock   (LISP)  JonRock is from USA
(defun fill-triangle-hash (lim)
  (loop for i from 1 to lim
     do (let ((tri (* i (1+ i) 1/2)))
	  (incf (gethash tri figure-hash 0)))))

(defun fill-pentagon-hash (lim)
  (loop for i from 1 to lim
     do (let ((tri (* i (1- (* 3 i)) 1/2)))
	  (incf (gethash tri figure-hash 0)))))

(defun fill-hexagon-hash (lim)
  (loop for i from 1 to lim
     do (let ((tri (* i (1- (* 2 i)))))
	  (incf (gethash tri figure-hash 0)))))

(defun euler29 ()
  (setq figure-hash (make-hash-table))
  (let ((limit 100000))
    (fill-triangle-hash limit)
    (fill-pentagon-hash limit)
    (fill-hexagon-hash limit))
  (loop for key being each hash-key of figure-hash
     if (>= (gethash key figure-hash) 3)
     collect key)) 
