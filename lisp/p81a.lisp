#|
23 Jan 2006 09:10 pm 
Peatey
|#
	

(defun collapse-square-matrix (arr n)
  (if (not (array-in-bounds-p arr n n))
      nil
    (if (= 0 n)
        (if (array-in-bounds-p arr (+ n 1) (+ n 1))
            (incf (aref arr n n)
                  (min (aref arr (+ n 1) n)
                       (aref arr n (+ n 1))))
          (aref arr n n))
      (progn
        (if (not (array-in-bounds-p arr (+ n 1) (+ n 1)))
            (do ((i 1 (+ i 1))) ((> i n) 'done)
              (incf (aref arr (- n i) n)
                    (aref arr (- n i -1) n))
              (incf (aref arr n (- n i))
                    (aref arr n (- n i -1))))
          (progn
            (incf (aref arr n n)
                  (min (aref arr (+ n 1) n)
                       (aref arr n (+ n 1))))
            (do ((i 1 (+ i 1))) ((> i n) 'done)
              (incf (aref arr n (- n i))
                    (min (aref arr n (- n i -1))
                         (aref arr (+ n 1) (- n i))))
              (incf (aref arr (- n i) n)
                    (min (aref arr (- n i -1) n)
                         (aref arr (- n i) (+ n 1)))))))
        (collapse-square-matrix arr (- n 1))))))