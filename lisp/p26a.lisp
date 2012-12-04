;; 20 Feb 2006 09:08 pm 
;; phil_g
;; LISP 
;; USA	
;;    Quote    0

;; From a bit of research on mathworld, I learned that, for fractions 1/n, the decimal representation only repeats for n that are relatively prime to 10 (makes sense). Furthermore, the number of repeating digits is equal to the multiplicative order of 10 modulo n. Finally, the maximum number of repeating digits is n-1. Thus: 

(defun multiplicative-order (k n &rest rs)
  "Gives the multiplicative order of K modulo N; or, the
   smallest integer M such that (= (mod (expt K M) N) R)
   for some R.  If no Rs are provided, 1 is used."
  (let ((rs (or rs '(1))))
    (loop for m from 1
          when (member (mod (expt k m) n) rs :test #'=)
            do (return m))))

(loop for i from 1000 downto 2
      with maxlen = 0
      with max-i = 0
      while (< maxlen i)
      when (and (= 1 (gcd i 10))
                (< maxlen (multiplicative-order 10 i)))
        do (setf maxlen (multiplicative-order 10 i)
                 max-i i)
      finally (return max-i))