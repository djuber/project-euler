
;; The fraction ^(49)/_(98) is a curious fraction, as an inexperienced
;; mathematician in attempting to simplify it may incorrectly believe
;; that ^(49)/_(98) = ^(4)/_(8), which is correct, is obtained by
;; cancelling the 9s.

;; We shall consider fractions like, ^(30)/_(50) = ^(3)/_(5),
;; to be trivial examples.

;; There are exactly four non-trivial examples of this type of fraction,
;; less than one in value, and containing two digits in the numerator and
;; denominator.

;; If the product of these four fractions is given in its lowest common terms,
;; find the value of the denominator.

(defun find-curious ()
  (let ((result ()))
    (do ((numer 1 (1+ numer)))
	((= numer 10))
      (do ((denom (1+ numer) (1+ denom)))
	  ((= denom 10))
	(do ((numer2 1 (1+ numer2)))
	    ((= numer2 10))
	  (do ((denom2 1 (1+ denom2)))
	      ((= denom2 10))
	    (if (curious-p numer numer2 denom denom2)
		(push (list numer numer2 denom denom2) result))))))
    result))

(defun curious-p (numer numer2 denom denom2)
  (if (or (char= #\0 (elt (format nil "~a" numer) 1))
	  (char= #\0 (elt (format nil "~a" denom) 1)))
      nil
      (if (and (not (and (zerop (mod numer 11))
			 (zerop (mod denom 11))))
	       (= (/ numer denom)
		  (/ (mod numer 10)
		     (mod denom 10))))
	  T
	  nil)))
      
    