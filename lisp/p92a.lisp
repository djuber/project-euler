#|
31 Jan 2006 02:53 am 
Peatey
LISP 
USA	

Common Lisp, brute force:
	
|#

(defun euler92 (&optional
                (n (expt 10 7))
                (upper (* 9 9 7)))
  (let ((vec (make-array (+ upper 1)
                         :initial-element nil))
        (total 0))
    (setf (svref vec 1) 0)
    (setf (svref vec 89) 1)
    (dotimes (i n total)
      (incf total (n-chain vec (list (+ i 1)) upper)))))

(defun n-chain (vec lst upper)
  (cond ((and (<= (car lst) upper)
              (svref vec (car lst)))
         (dolist (num (cdr lst))
           (when (<= num upper)
             (setf (svref vec num) (svref vec (car lst)))))
         (svref vec (car lst)))
        (t
         (push (sum-square-digits (car lst)) lst)
         (n-chain vec lst upper))))

(defun sum-square-digits (n)
  (let ((total 0))
    (do ((i n (floor i 10))) ((zerop i) total)
      (let ((r (mod i 10)))
        (incf total (* r r))))))