;; 17 Oct 2005 12:31 pm 
;; tomjen   (LISP)  
;; from the problem 1 forum
;; (add all numbers less than 1000 divisible by 3 or 5)
(loop for i from 1 to 999
   when (zerop (mod i 3)) sum i
   when (zerop (mod i 5)) sum i
   when
     (and
      (zerop (mod i 5))
      (zerop (mod i 3))) sum (* -1 i))

;; (c-x c-e to evaluate)

;; next version
(defun sum-of-3-or-5 (limit)
  (loop for i from 1 to limit
     if (or (= 0 (mod i 3))
	    (= 0 (mod i 5)))
     sum i))

(defun euler1 ()
  (sum-of-3-or-5 999)) 
;(euler1) ;(c-x c-e again)

;; next version
(let ((a (loop for i from 3 to 999 by 3 collect i))
      (b (loop for i from 5 to 999 by 5 collect i)))
  (apply #'+ (remove-duplicates (append a b))))
;; this works as well

;; another version

(defun problem-1 ()
  (loop for x from 3 to 999
     when (or
	   (zerop (mod x 3))
	   (zerop (mod x 5))) sum x))
; (problem-1)

;; someone did it like I did
(defun euler1 (&optional (n 1000))
  (let ((total 0))
    (do ((i 1 (+ i 1))) ((= i n) total)
      (if (or (zerop (mod i 3))
              (zerop (mod i 5)))
          (incf total i)))))
(euler1 1000)