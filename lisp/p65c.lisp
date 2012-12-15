;; 24 Mar 2006 06:40 am 
;; JonRock
;; LISP 
;; USA	
;;    Quote    0


(defun digits (num) 
  (map 'list 
       #'(lambda (char) 
	   (read-from-string (string char))) 
       (prin1-to-string num))) 


(defun continued-fraction (residuals) 
  (if (endp (cdr residuals)) 
      (car residuals)
      (+ (car residuals) 
	 (/ 1 (continued-fraction (cdr residuals)))))) 

(defun e-residuals (limit) 
  (loop for i from 1 to limit 
     collecting (cond ((= i 1) 2) ((= (mod i 3) 0) (* 2/3 i)) (t 1)))) 

(defun euler65 () 
  (reduce #'+ (digits (numerator (continued-fraction (e-residuals 100))))))
