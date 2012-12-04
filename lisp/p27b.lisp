#|

07 Oct 2011 09:09 pm 
tomalmy
LISP 
USA	
   Quote    0
XLISP-PLUS Brute force, using my prime number server since we know that b must be prime. We also know that a must be odd, and greater than or equal to 2-b, and can save some time there.

|#


(defun primelength (a b)
       (do* ((i 0 (1+ i))
             (val b (+ (* (+ i a) i) b)))
            ((or (< val 0) (null (primep val)))
             i)))


(defun euler27 ()
       (let (besta bestb (bestlen 0))
            (do ((b 3 (nextprime b)))
                ((> b 1000))
                (do* ((a (- 2 b) (+ 2 a)))
                     ((>= a 1000))
                     (let ((pl (primelength a b)))
                          (when (> pl bestlen)
                                (setf besta a
                                      bestb b
                                      bestlen pl)))))
            (* besta bestb)))