(defun list-divisors (n)
  (loop for i from 1 to (sqrt n)
     when (= (mod n i) 0)
     collect i
     and unless (= i (/ n i))
     collect (/ n i)))

(defun triangle (n)
  (/ (* n (+ n 1)) 2))

(defun triangle-factors (limit)
  (loop for n from 1
     for tri = (triangle n)
     when (> (length (list-divisors tri)) limit)
     return tri))

(defun euler12 ()
  (triangle-factors 500)) 
