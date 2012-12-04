;; phil_g

(defun find-summing-combination-count (sum multipliers)
  (declare (type integer sum)
           (type (or cons null) multipliers))
  (if (endp multipliers)
      (if (zerop sum)
          1
          0)
      (let ((mult (car multipliers)))
        (loop for coeff from 0 to (floor sum mult)
              sum (find-summing-combination-count
                   (- sum (* coeff mult))
                   (cdr multipliers))))))

(find-summing-combination-count 200 '(200 100 50 20 10 5 2 1))