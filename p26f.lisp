#|
17 Sep 2007 08:37 pm 
fred-o
LISP 
Sweden	
   Quote    0

This is a fairly compact Common Lisp solution. It relies on the way Lisp handles rational numbers; 1/7 is really stored as 1/7 and not 0.1428571... It's not optimized at all; I could cut shave some clock cycles off by not checking the length of multiples of previously checked numbers, but why bother? It runs in half a second anyway on my machine... :D
	
|#

(defun cycle-length (number)
  (loop
     for fraction = number then (mod (* fraction 10) 1)
     for i = (position fraction previous-fractions)
     when i return (- (length previous-fractions) i)
     collecting fraction into previous-fractions))

(defun euler-proj-26 ()
  (let* ((lengths (loop for i from 1 to 1000 collecting (cycle-length (/ 1 i))))
         (pos (position (reduce #'max lengths) lengths)))
    (1+ pos)))