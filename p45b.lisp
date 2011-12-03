(defun euler45 (&optional (h 144))
  (let* ((hn (* h (- (* 2 h) 1)))
         (tn (/ (+ -1 (sqrt (+ 1 (* 8 hn)))) 2))
         (pn (/ (+ 1 (sqrt (+ 1 (* 24 hn)))) 6)))
    (if (and (integerp tn) (integerp pn))
        hn
        (euler45 (+ h 1)))))
