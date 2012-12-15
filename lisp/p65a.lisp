#|
23 Jan 2006 11:58 pm 
Peatey
LISP 
USA	
|#
	

(defun euler65 (&optional (n 100))
  (sum-digits-str
   (format nil "~A" (numerator
                     (fraction-for-e 0 n)))))

(defun fraction-for-e (begin end)
  (cond ((= begin end)
         0)
        ((zerop begin)
         (+ 2 (fraction-for-e (+ 1 begin) end)))
        ((= 2 (mod begin 3))
         (/ 1 (+ (* 2 (/ (+ 1 begin) 3))
                 (fraction-for-e (+ 1 begin) end))))
        (t
         (/ 1 (+ 1 (fraction-for-e (+ 1 begin) end))))))

(defun sum-digits-str (str)
  (if (zerop (length str))
      0
    (+ (- (char-code (elt str 0)) 48)
       (sum-digits-str (subseq str 1)))))
