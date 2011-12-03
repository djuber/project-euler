(defun triangle (n)
  (/ (* n (1+ n)) 2))
 
(defun pentagonal (n)
  (/ (* n (1- (* 3 n))) 2))
 
(defun hexagonal (n)
  (* n (1- (* 2 n))))
 
(define-symbol-macro tri-v (triangle tri))
(define-symbol-macro pen-v (pentagonal pen))
(define-symbol-macro hex-v (hexagonal hex))
 
(defun equals (list)
  (every 'equal list (rest list)))
 
 
(do ((tri 286 (if (= tri-v (min tri-v pen-v hex-v)) (1+ tri) tri))
     (pen 166 (if (= pen-v (min tri-v pen-v hex-v)) (1+ pen) pen))
     (hex 144 (if (= hex-v (min tri-v pen-v hex-v)) (1+ hex) hex)))
    ((equals (list tri-v pen-v hex-v)) (format t "~a ~a ~a~%" tri pen hex)))
