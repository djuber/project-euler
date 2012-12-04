;; 26 Oct 2005 05:51 pm 
;; JonRock
;; LISP 
;; USA	



(defun factorial (n) 
  (if (= n 0) 
      1 
      (* n (factorial (- n 1))))) 

(defun but-nth (n lst) 
  (if (= n 0) 
      (cdr lst) 
      (cons (car lst) 
	    (but-nth (- n 1) (cdr lst))))) 

(defun permute (n lst) 
  (if (= n 0) 
      lst 
      (let* 
	  ((len (length lst)) 
	   (sublen (- len 1)) 
	   (modulus (factorial sublen))) 
	(if (> n (* len modulus)) 
	    (error "List of length ~A doesn't have ~A permutations." len n) 
	    (multiple-value-bind (quotient remainder) 
		(floor n modulus) 
	      (cons (nth quotient lst) 
		    (permute remainder (but-nth quotient lst)))))))) 


(permute 999999 '(0 1 2 3 4 5 6 7 8 9))