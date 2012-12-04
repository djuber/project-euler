
;; 06 Apr 2007 12:28 am 
;; haroldl

;;Same as #18, except with a diamond instead of a triangle. I love that Common Lisp allows me to read in the input file using the Lisp scanner/parser, and I can even change comma (,) to be whitespace for the scanner:


(defun read-matrix ()
  (let ((matrix (make-array '(80 80)))
        (*readtable* (copy-readtable)))
    (set-syntax-from-char #\, #\Space)
    (with-open-file (s (merge-pathnames #P"euler-matrix.txt"))
      (iter (for i from 0 below 80)
            (iter (for j from 0 below 80)
                  (setf (aref matrix i j) (read s)))))
    matrix))


;;Then I do the predictable but boring summation to get the answer (modifying the matrix in place).


(defun make-path-sums (&optional (matrix (read-matrix)))
  "Walk the matrix and update shorted path sums, allow down/right moves."
  (destructuring-bind (width height)
      (array-dimensions matrix)
    ;; Bottom row, you can only move right.
    (iter (for j from (- width 2) downto 0)
          (incf (aref matrix (1- height) j) 
                (aref matrix (1- height) (1+ j))))
    ;; Right column, you can only move down.
    (iter (for i from (- height 2) downto 0)
          (incf (aref matrix i (1- width))
                (aref matrix (1+ i) (1- width))))
    ;; The rest of the time, you can move right or down.
    (iter (for i from (- height 2) downto 0)
          (iter (for j from (- width 2) downto 0)
                (incf (aref matrix i j)
                      (min (aref matrix (1+ i) j)
                           (aref matrix i (1+ j)))))))
  matrix)

(defun euler-81 ()
  (aref (make-path-sums) 0 0))