(defun p48 ()
  "find the last ten digits in the sum over a^a from a=1 to 1000"
  (let ((s 0) ; the sum
	(modulus 10000000000)) ; ten digits
    (do ((i 1 (1+ i)))
	((> i 1000))
      (incf s (mod (expt i i) modulus)))
    (mod s modulus)))
