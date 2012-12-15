#|
22 Jan 2006 02:26 pm 
jsn
Perl 
Russia	
 |#

;; scheme48 
;; (define (seq n) 
;;     (if (= 0 (remainder n 3)) 
;; 	(* 2 (quotient n 3)) 1)) 

(defun seq (n)
  (if (zerop (mod n 3))
      (* 2 (/ n 3))
      1))

;;(define (calc n ac) (/ 1 (+ (seq ac) (if (= n ac) 0 (calc n (+ 1 ac)))))) 

(defun calc (n ac)
  (/ 1 (+ (seq ac)
	  (if (= n ac) 
	      0
	      (calc n (1+ ac))))))

;;(define (enumerator n) (numerator (+ 1 (calc n 0)))) 

(defun enumerator (n)
  (numerator (+ 1 (calc n 0))))

;;(define (digit->integer x) (- (char->integer x) (char->integer #\0))) 
(defun digit->integer (x)
  (- (char-code x) (char-code #\0)))

(defun p65b ()
  (apply #'+
	 (mapcar #'digit->integer
		 (coerce (format nil "~a" (enumerator 100)) 
			 'list))))

;;(apply + (map digit->integer (string->list (number->string (enumerator 100))))) 
