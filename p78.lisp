;; find the least value of n for which p(n) is divisible by one million.
;; this could be faster by taking mod 1000000 each time, rather than calculating whole values.

(defun p78 ()
  (let ((numbers (numbparts 100000 :all t)))
    (let ((zeros (map 'simple-vector (lambda (x) (mod x 1000000)) numbers)))
      (loop for i from 0 to 100000
	   when (= 0 (aref zeros i))
	   collect (cons i (aref numbers i))))))

;; general partition function p(n), adapted from R partitions package below
(defun numbparts (n &key (all nil))
  (when (< n 2) (return-from numbparts 1))
  (let ((p (make-array (1+ n) :element-type 'integer :initial-element 0))
	(pp (make-array (1+ n) :element-type 'integer :initial-element 0)))
    (labels 
	((outerloop (i)
	   (let ((s 1) ;; sign
		 (f 5) ;; first difference
		 (r 2)) 
	     ;; while loop 1
	     (loop until (< i r)
		do (progn
		     (incf (aref pp i) (* s (aref pp (- i r))))
		     (incf r f)
		     (incf f 3)
		     (setf s (* s -1))))
	     (setf s 1)
	     (setf f 4)
	     (setf r 1)
	     ;; while loop 2
	     (loop until (< i r)
		  do (progn
		       (incf (aref pp i) (* s (aref pp (- i r))))
		       (incf r f)
		       (incf f 3)
		       (setf s (* s -1)))))))
      (setf (aref p 0) 1 (aref p 1) 1 (aref pp 0) 1 (aref pp 1) 1)
      (loop for i from 2 to  n
	 do (outerloop i))
      (if all
	  pp
	  (aref pp n)))))
#|
void numbparts(const int *n, double *p){/* p(1)...p(n) calculated using
                     Euler's recursive formula on p825
                     of Abramowitz & Stegun */
    int i, s, f, r;
    unsigned long long int *ip;
    unsigned long long int pp[*n];

    pp[0] = pp[1] = 1;
    for(i=2 ; i< *n ; i++){
        /* first do r = m(3m+1)/2  */
        s = 1;  /* "s" for "sign" */
        f = 5;  /* f is first difference  */
        r = 2;  /* initial value (viz m(3m+1)/2 for m=1) */

        ip = pp+i;
        *ip = 0;
        while(i-r >= 0){
            *ip += s*pp[i-r];
            r += f;
            f += 3;  /* 3 is the second difference */
            /* change sign of s */
            s *= -1;
        }
        /* now do r = m(3m-1)/2 */
        s = 1;
        f = 4; /*first difference now 4 */
        r = 1; /* initial value (viz m(3m-1)/2 for m=1) */
        while(i-r >= 0){
            *ip += s*pp[i-r];
            r += f;
            f += 3;
            s *= -1;
        }
    }
    for(i=0 ; i < *n ; i++){
                p[i] = (double) pp[i];
        }
   
}
|#