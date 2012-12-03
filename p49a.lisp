#|
17 Aug 2006 12:48 pm 
phil_g
LISP 
USA	

I thought about the approach that everyone else seems to have taken (permutations of identical-digit primes), but couldn't think of a simple way to find arithmetic sequences. The approach I went for was simpler: start somewhere (above 1488 for the real answer), examine all of the possible addition sequences looking for primes and digit equality. In Common Lisp:

|#

(defun 049-get-answer (&optional (start 1488))
  (iter outer
        (for a first (next-prime start) then (next-prime a))
        (while (< a 10000))
        (for a-digits = (sort (digit-list a) #'<))
        (iter (for inc from 1 to (floor (- 9999 a) 2))
              (for b = (+ a inc))
              (for c = (+ b inc))
              (in outer
                  (finding (+ (* a 100000000) (* b 10000) c)
                           such-that (and (primep b)
                                          (primep c)
                                          (equal a-digits
                                                 (sort (digit-list b)
                                                       #'<))
                                          (equal a-digits
                                                 (sort (digit-list c)
                                                       #'<))))))))