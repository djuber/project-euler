#|
22 Mar 2006 02:17 am 
JonRock
LISP 
USA	
   Quote    0
phil_g: I would be interested in your digit-handling functions. I'm sure I'm being very inefficient by using string conversion for this purpose. However, most of the time in my solution is spent sieving up the list of primes so that my primep function will work. 
|#


(defun digits (num) 
  (map 'list 
       #'(lambda (char) (read-from-string (string char))) 
       (prin1-to-string num))) 

(defun digit-val (digits) 
  (read-from-string (format nil "~{~D~}" digits))) 

(defun seq-list (min max) 
  (loop for i from min to max collect i)) 

(defun sieve (lst) 
  (let ((primes '()) (last (car (last lst)))) 
    (loop while (and lst (> last (* (car lst) (car lst)))) do 
	 (let ((factor (car lst))) 
	   (setq primes (cons factor primes)) 
	   (setq lst (remove-if #'(lambda (n) (= (mod n factor) 0)) (cdr lst))))) 
    (append (reverse primes) lst))) 

(defun all-primes (limit) 
  (sieve (seq-list 2 limit))) 

(defun generate-all-primes (limit) 
  (setq all-primes (all-primes limit)) 
  (setq primehash (make-hash-table)) 
  (loop for p in all-primes do 
       (setf (gethash p primehash) p))) 

(defun primep (p) 
  (gethash p primehash)) 

(defun left-truncatable-p (n) 
  (and (primep n) 
       (or (< n 10) 
	   (left-truncatable-p (digit-val (cdr (digits n))))))) 

(defun right-truncatable-p (n) 
  (and (primep n) 
       (if (< n 10) 
	   (right-truncatable-p (digit-val (butlast (digits n))))))) 

(defun all-right-truncatable-primes (limit) 
  (generate-all-primes limit) 
  (let ((result '()) 
	(queue '(2 3 5 7))) 
    (loop while queue do 
	 (let ((cur (car queue))) 
	   (setq queue (cdr queue)) 
	   (if (> cur 10) 
	       (setq result (cons cur result))) 
	   (loop for d from 1 to 9 do 
		(let ((next (+ (* cur 10) d))) 
		  (if (primep next) 
		      (setq queue (cons next queue))))))) 
    result)) 
(defun all-bi-truncatable-primes (limit) 
  (remove-if-not #'left-truncatable-p 
		 (all-right-truncatable-primes limit))) 

(defun euler37 () 
  (reduce #'+ (all-bi-truncatable-primes 1000000))) 
